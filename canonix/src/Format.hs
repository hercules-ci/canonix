{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Format where

import           Canonix.Monad
import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as T
import           Control.Monad
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as BL
import           Data.Char                      ( ord )
import           Data.Monoid                    ( Ap(Ap) )
import           Data.Foldable
import           Data.Functor.Foldable
import           Data.Semigroup.Generic
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Word                      ( Word8 )
import           Foreign.Marshal.Alloc          ( malloc )
import           Foreign.Marshal.Array          ( peekArray
                                                , allocaArray
                                                )
import           Foreign.Marshal.Utils          ( with )
import           Foreign.Ptr                    ( nullPtr )
import           Foreign.Storable               ( peek )
import           GHC.Generics                   ( Generic )
import           System.IO
import           TreeSitter.Nix
import           TreeSitter.Node
import           TreeSitter.Parser
import           TreeSitter.Tree
-- import Debug.Trace (trace)

traceDemand :: String -> a -> a
traceDemand = flip const  -- folds away the debug statements
-- traceDemand = trace  -- should show an interleaving of Chunk production and Node demand

format :: Bool -> ByteString -> IO BL.ByteString
format debug source = do
  parser <- ts_parser_new
  _ <- ts_parser_set_language parser tree_sitter_nix

  BS.useAsCStringLen source $ \(str, len) -> do

    ttree       <- ts_parser_parse_string parser nullPtr str len

    n          <- malloc
    ts_tree_root_node_p ttree n

    root@Node {..} <- peek n

    let
      dump :: Int -> Node -> IO ()
      dump i nd = do
        printNode i source nd
        void $ forChildren nd $ \c ->
          dump (i+1) c
    when debug $ dump 0 root

    tree <- mkTree root
    let f = bottomUp (fmap (lg . abstract) tree) formatter
        lg nd = traceDemand ("Demanding " <> show nd) nd

    let go (Step chunk0 tl) = do  -- Either
           let chunk = traceDemand ("Chunk completed (" <> show chunk0 <> ")") chunk0
           -- Demanding chunks strictly should help to keep active memory small.
           -- They will be gathered up in memory, but that's unavoidable
           -- because checking for exceptions is strict and we don't want/need
           -- to write partial files.
           --
           -- When this statement is omitted, the pattern match on the final
           -- Either will cause the whole Fmt to be loaded into memory, which
           -- is a big data structure with many thunks in it. Makes those few
           -- output bytes insignificant.
           chunk `seq` pure ()
           (cs, r) <- go tl
           let cs' = BB.byteString chunk <> cs

           -- Either of these seqs should independently do the trick, but
           --   - the one above makes trace message interleaving nicely granular
           --   - this one compacts the loose chunks into contiguous memory
           cs' `seq` pure ()
           pure (cs', r)
        go (Exceptional e) = Left e
        go (Done r) = pure (pure r)

    (blob, r) <- case go (runFmt (snd f) $ rootInherited source) of
      Left e -> error e  -- TODO use proper IO exception instead of pure exception
      Right x -> pure x

    let synthesized = resultSyn r

    -- NB: debug forces the tree before the lazy bytestring does it
    when debug $ do
      hPutStrLn stderr $ "Encountered these unknown node types: " <> show (unknownTypes synthesized)
      hPutStrLn stderr $ "Verbatim fallback nodes: " <> show (fallbackNodes synthesized)

    pure $ BB.toLazyByteString blob

bottomUp :: Cofree [] ANode -> (ANode -> [(ANode, a)] -> a) -> (ANode, a)
bottomUp t f = cataA (\(self T.:< c) -> (self, f self c)) t

-- | "Abstract" node
--
-- TODO: Put in separate module and just call it Node
data ANode = ANode
  { startByte :: {-# UNPACK #-}!Int
  , endByte :: {-# UNPACK #-}!Int
  , typ :: !Grammar
  , isMultiline :: !Bool -- TODO: maybe preserve line numbers instead so we can figure out which parts of a construct are multiline
  } deriving (Eq, Ord, Show)

abstract :: Node -> ANode
abstract n =
  ANode
    { typ = toEnum (fromEnum (nodeSymbol n))
    , startByte = fromIntegral $ nodeStartByte n
    , endByte = fromIntegral $ nodeEndByte n
    , isMultiline = pointRow (nodeStartPoint n) /= pointRow (nodeEndPoint n)
    }

type ErrorMessage = String

type CnxFmt = Fmt Inherited Synthesized ByteString ErrorMessage

-- | Runs its argument only if outputting a single line is a possibility.
whenSingleLineAllowed :: CnxFmt a -> CnxFmt (Maybe a)
whenSingleLineAllowed fmt = do
  allowed <- asksParent singleLineAllowed
  forM (guard allowed) (const fmt)

-- | Declare that the single-line format has failed, ensuring that the
-- multiline-style output will be written.
forceMultiline :: CnxFmt ()
forceMultiline = tellParent mempty { singleLine = Ap Nothing }

-- | Try to use the single line format. This is how the multiline format can
-- use the single line format.
trySingleLine :: CnxFmt a -> CnxFmt a
trySingleLine fmt = do
  allowed <- asksParent singleLineAllowed
  if not allowed then fmt
  else do
    rw      <- asksParent remainingWidth
    censorWrites (censorChildren fmt $ \syn a -> do
      let
        ln :: Ap Maybe ByteString
        ln = do
          l0 <- singleLine syn
          -- rely on fusion
          let l1 = BS.reverse . BS.dropWhile (== charCode ' ') . BS.reverse $ l0
          guard (BS.length l1 <= rw) -- TODO: unicode width
          pure l1

      (ln, a) <$ tellParent (syn { singleLine = ln })
      ) $ \multilineChunks (ln, a) -> do
      case ln of
        Ap (Just l) -> write l
        _ -> mapM_ write multilineChunks
      pure a

-- | Tell the children what they need to know from their parent.
withSelf :: ANode -> CnxFmt () -> CnxFmt ()
withSelf self fmt = do
  inh <- askParent
  tellChildren inh { singleLineAllowed = not (isMultiline self) } fmt

matchOneOfTypeWithComments :: Applicative m => [(ANode, m a)] -> Maybe (Grammar, m a, [(ANode, m a)])
matchOneOfTypeWithComments = go id
  where
    go acc ((n, a) : as) | typ n == Comment = go ((a *>) . acc) as
    go acc ((t, a) : as) = Just (typ t, (acc a), as)
    go _ [] = Nothing

matchOneWithComments :: Applicative m => (ANode -> Bool) -> [(ANode, m a)] -> Maybe (Grammar, m a, [(ANode, m a)])
matchOneWithComments p = go id
  where
    go acc ((n, a) : as) | p n = Just (typ n, (acc a), as)
    go acc ((n, a) : as) | typ n == Comment = go (acc . (a *>)) as
    go _ _ = Nothing

matchManyOfTypeWithComments :: Applicative m => (ANode -> Bool) -> [(ANode, m a)] -> ([(Grammar, m a)], [(ANode, m a)])
matchManyOfTypeWithComments p = go id
  where
    go acc l = case matchOneWithComments p l of
                      Just (g, m, rest) -> go (acc . ((g, m):)) rest
                      Nothing -> (acc [], l)

pattern One :: Applicative m => Grammar -> m a -> [(ANode, m a)] -> [(ANode, m a)]
pattern One t m rest <- (matchOneOfTypeWithComments -> Just (t, m, rest))

spanTypes :: Applicative m => [Grammar] -> [(ANode, m a)] -> ([(Grammar, m a)], [(ANode, m a)])
spanTypes ts = matchManyOfTypeWithComments (\x -> typ x `elem` ts)

pattern Comments :: Applicative m => [(Grammar, m a)] -> [(ANode, m a)] -> [(ANode, m a)]
pattern Comments cs rest <- (spanTypes [Comment] -> (cs, rest))

pattern (:*:) :: a -> b -> (a, b)
pattern a :*: b = (a, b)

formatter :: ANode -> [(ANode, CnxFmt ())] -> CnxFmt ()
formatter self children = withSelf self $ trySingleLine $
  case (typ self, children) of
            -- Expression is the root node of a file
            (Expression, _) -> do
              mconcat <$> traverse snd children
              forceNewline

            (Function,
              One Formals formals (One AnonColon colon (One _ body []))) -> do
              formals
              colon
              withOptionalIndent 2 body

            (Function,
              One Identifier i (One AnonColon colon (One Function body []))) -> do
              i
              colon
              space
              body

            (Function, One Identifier i (One AnonColon colon (One _ body []))) -> do
              i
              colon
              withOptionalIndent 2 body

            (Formals, One AnonLBracket open rest) -> do
              open
              traverse_ snd rest

            (Formal, One Identifier i []) ->
              i

            (Formal, One Identifier i (One AnonQuestion q (One _ e []))) -> do
              i
              space
              q
              space
              e

            (App, [(_, a),(_, b)]) -> do
              a
              space
              b

            (Let,
              One Let l
                (spanTypes [Bind, Inherit] -> bindings :*:
                  (One AnonIn inkw
                    (One bodyTyp body [])
                  )
                )
              ) -> do
              l
              withIndent 2 $
                traverse_ snd bindings
              newline
              inkw
              if bodyTyp == Attrset
              then do
                space
                body
              else
                withIndent 2 body

            (Bind, One Attrpath a (One AnonEqual eq1 (One _ v (One AnonSemicolon sc [])))) -> do
              a
              space
              eq1
              withIndent 2 $ do
                v
                sc
              newline

            (Inherit, (One Inherit inhKw (One Parenthesized p (One Attrs attrs (One AnonSemicolon sc []))))) -> do
              inhKw
              space
              p
              withIndent 2 $ do
                space
                attrs
              sc
              newline

            (Inherit, (One Inherit inhKw (One Attrs attrs (One AnonSemicolon sc [])))) -> do -- FIXME specific attrs
              inhKw
              withIndent 2 $ do
                space
                attrs
              sc
              newline

            (Attrset,
              One AnonLBrace open
                (spanTypes [Bind, Inherit] -> bindings :*:
                  Comments finalComments (
                    One AnonRBrace close []
                  )
                )
              ) -> do
              open
              withIndent' 2 $ do
                forM_ bindings $ \(_, i) -> do
                  newline
                  i
                mapM_ snd finalComments
              newline -- ??
              close

            (Attrs, as) | all (\(x, _) -> typ x == Identifier) as ->
              forM_ as $ \(_, i) -> do
                space
                i

            (AnonRBracket, []) -> verbatim self
            (AnonLBracket, []) -> verbatim self
            (AnonRBrace, []) -> verbatim self
            (AnonLBrace, []) -> verbatim self
            (AnonColon, []) -> verbatim self
            (Identifier, []) -> verbatim self
            (AnonEqual, []) -> verbatim self
            (AnonIn, []) -> verbatim self
            (Inherit, []) -> verbatim self
            (Let, []) -> verbatim self
            (AnonQuestion, []) -> verbatim self
            (AnonSemicolon, []) -> verbatim self
            (Spath, []) -> verbatim self
            (Integer, []) -> verbatim self
            (Comment, []) -> do
              forceMultiline
              newline -- TODO: clearline
              verbatim self
              newline
            (_x, _y) -> do
              tellParent mempty { fallbackNodes = S.singleton self }
              verbatim self

-- | A newline in multiline format, but a space in single line format
newline :: CnxFmt ()
newline = do
  ind <- asksParent indent
  write "\n"
  write (BS.replicate ind (charCode ' '))
  void $ whenSingleLineAllowed $
    tellParent mempty { singleLine = pure " " }

-- This is only used by Expression (top-level)
-- A better way to solve this is add the final newline outside the recursive
-- format function.
forceNewline :: CnxFmt ()
forceNewline = do
  ind <- asksParent indent
  write "\n"
  write (BS.replicate ind (charCode ' '))
  void $ whenSingleLineAllowed $
    tellParent mempty { singleLine = pure "\n" }

-- | Copy a node to the output without any changes
verbatim :: ANode -> CnxFmt ()
verbatim n = do
  src <- asksParent source
  let bs = BS.drop (startByte n) (BS.take (endByte n) src)
  write bs
  _ <- whenSingleLineAllowed $ do
    rw <- asksParent remainingWidth
    tellParent mempty { singleLine = bs <$ guard (BS.length bs <= rw) }
  pure ()

-- | Just a space
space :: CnxFmt ()
space = do
  write " "
  void $ whenSingleLineAllowed $
    tellParent mempty { singleLine = pure " " }

-- | Adds a newline and starts an indented block.
withIndent :: Int -> CnxFmt a -> CnxFmt a
withIndent n m = do
  inh <- askParent
  tellChildren (inh { indent = indent inh + n }) (newline *> m)

-- | Declare increased indentation while appending to the current line
withIndent' :: Int -> CnxFmt a -> CnxFmt a
withIndent' n m = do
  inh <- askParent
  tellChildren (inh { indent = indent inh + n }) m

-- | Indent except at top level.
--
-- Always creates a new line, like 'withIndent'.
withOptionalIndent :: Int -> CnxFmt a -> CnxFmt a
withOptionalIndent n m = do
  tl <- asksParent indent
  if (tl == 0) then (newline *> m) else withIndent n m

charCode :: Char -> Word8
charCode = fromIntegral . ord

data Inherited = Inherited
  { indent :: Int
  , source :: ByteString
  , singleLineAllowed :: Bool
  }
rootInherited :: ByteString -> Inherited
rootInherited bs = Inherited { indent = 0, source = bs, singleLineAllowed = False }

remainingWidth :: Inherited -> Int
remainingWidth inh = max 30 (78 - indent inh)

data Synthesized = Synthesized
  { unknownTypes :: Set String
  , fallbackNodes :: Set ANode
  , singleLine :: Ap Maybe ByteString
    -- ^
    -- singleLine represents a formatted single-line variation of the subtree, or
    -- Nothing when the line is too long, or if the original tree is multiline.
    --
    -- When a node isn't already known to be multiline because of multiline
    -- input, this attribute is required for determining whether it should be,
    -- based on the size of the subexpressions. That is - by checking its
    -- own single-line output before streaming out the multiline output.
  }
  deriving (Generic, Show)
instance Semigroup Synthesized where (<>) = gmappend
instance Monoid Synthesized where { mappend = gmappend; mempty = gmempty }

    -- TODO: Turn these considerations into design document
    --------------------------------------------------------------------------
    --
    -- Previous notes (for context, possibly outdated):
    --
    -- (we used to have out :: ByteString.Builder here in Synthesized)
    --
    -- We can avoid a full blown pretty printing library, because
    -- those are designed to do fancy 2D layouts, yet they are too syntax
    -- directed. What we typically want for merge-friendly layout is at
    -- most two layouts per given node type. A single-line one and a
    -- multiline one. Expanding all ancestry to multi-line layout greedily
    -- whenever a line is full seems like a good idea:
    --   - multi-line is good for merges, so we want more of it
    --   - greedy is good because it reduces the number of possible
    --     solutions for a given source text, reducing conflicts
    --   - if someone edits the single line without having had to expand it
    --     we would have had a merge conflict anyway
    --
    -- So it's important here that a single vs. multiline decision only
    -- affects a single "single" line - not this:
    --
    --  {                          {
    --    a = lineTooLong;           a =
    --    b = short;                   oneOrMoreLines;
    --  }                            b =
    --    potential extra   ------>    short;
    --       conflict              }
    --
    -- The right hand side looks nicer, but does introduce an extra conflict
    -- if someone else edited b before lineTooLong became too long.
    --
    -- Another interesting thing here is what to do if oneOrMoreLines is now
    -- a single line, due to the extra horizontal space from the multiline-style
    -- attribute.
    -- The solution is simple: never go back to the single line
    -- format. The attribute node ("bind") should detect that it's multiline
    -- and stay that way. Of course the user is free to manually make it
    -- compact.
    -- This can be implementated by the formatting abstraction: it can check
    -- whether all Nodes it contains are on the same line and if not, force
    -- the multiline layout.
    --
    -- The way to specify the layout is probably
    --   space :: m ()   -- non-breaking
    --   brSpace :: m () -- breakable space
    --   line :: m ()    -- always a line break
    --   layer :: m () -> m () -- delimiter to scope the choice of
    --                            single-line vs multi-line
    --                            Probably best to put a call to this around
    --                            the result of every input node, so around the
    --                            `formatter` function.
    --
    -- Back to the choice of abstraction: we want an abstraction that
    -- can choose between alternatives we explicitly specify instead of
    -- having the abstraction decide at its node level which is typically
    -- too fine grained.
    -- We've been working on ByteStrings because that's what both Nix and
    -- tree-sitter do. Sticking to ByteStrings may be nice. We can still
    -- count UTF8 code points to make it nice. (I know code points aren't
    -- glyphs, but you have to draw a line - what's the width of a 'glyph'
    -- anyway?)

-- Cofree [] Node: rose tree (n-ary tree) with Node as the label for each node
mkTree :: Node -> IO (Cofree [] Node)
mkTree n = (n :<) <$> mkChildren
  where mkChildren = forChildren n $ \c -> mkTree c

forChildren :: Node -> (Node -> IO a) -> IO [a]
forChildren n f = do
  let count = fromIntegral $ nodeChildCount n
  children <- allocaArray count $ \childNodesPtr -> do
    _ <- with (nodeTSNode n) (`ts_node_copy_child_nodes` childNodesPtr)
    peekArray count childNodesPtr
  forM children f

nodeInner :: BS.ByteString -> Node -> BS.ByteString
nodeInner bs n =
   BS.drop (fromIntegral $ nodeStartByte n)
   $ BS.take (fromIntegral $ nodeEndByte n) bs

printNode :: Int -> BS.ByteString -> Node -> IO ()
printNode ind source n@Node {..} = do
  let start =
        let TSPoint {..} = nodeStartPoint
        in  "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
      end =
        let TSPoint {..} = nodeEndPoint
        in  "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
      typ :: Grammar
      typ = toEnum $ fromEnum $ nodeSymbol
  hPutStrLn stderr $ replicate (ind*2) ' ' ++ show typ ++ start ++ "-" ++ end
  hPutStrLn stderr $ replicate (ind*2) ' ' ++ show (nodeInner source n)
