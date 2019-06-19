{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Format where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.ByteString.Builder        ( Builder )
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.UTF8          as BU
import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.Nix
import           TreeSitter.Node
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr                    ( nullPtr )
import           Foreign.Marshal.Alloc          ( malloc
                                                )
import           Foreign.Marshal.Array          ( peekArray
                                                , allocaArray
                                                )
import           Foreign.Marshal.Utils          ( with )
import           Foreign.Storable               ( peek )
import           System.IO.Unsafe

import           Foreign.ForeignPtr
import           Foreign.C
import           System.IO

-- recursion
import           Control.Monad.Identity
import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as T
import           Data.Functor.Foldable

-- data flow
import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.State

import           GHC.Generics
import           Data.Semigroup.Generic
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import Data.Char (ord)
import Data.String
import Data.List ((\\))

format :: Bool -> ByteString -> IO ByteString
format debug source = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_nix

  BS.useAsCStringLen source $ \(str, len) -> do

    tree       <- ts_parser_parse_string parser nullPtr str len

    n          <- malloc
    ts_tree_root_node_p tree n

    root@Node {..} <- peek n

    let
      dump :: Int -> Node -> IO ()
      dump i n = do
        printNode i source n
        void $ forChildren n $ \c ->
          dump (i+1) c
    when debug $ dump 0 root

    tree <- mkTree root
    let f = bottomUp (fmap abstract tree) formatter
        (_result, lastPreceding, synthesized) = f (rootInherited source) rootPreceding
    when debug $ do
      hPutStrLn stderr $ "Encountered these unknown node types: " <> show (unknownTypes synthesized)
      hPutStrLn stderr $ "Verbatim fallback nodes: " <> show (fallbackNodes synthesized)
    pure $ BL.toStrict $ BB.toLazyByteString $ out synthesized

bottomUp :: (Traversable f, Monoid w) => Cofree f a -> (a -> f (a, RWS inherited w preceding b) -> RWS inherited w preceding b)
  -> inherited -> preceding -> (b, preceding, w)
bottomUp t f = runRWS $ snd $ (cataA (\(self T.:< c) -> (self, f self c)) t)

-- | "Abstract" node
--
-- TODO: Put in separate module and just call it Node
data ANode = ANode
  { startByte :: {-# UNPACK #-}!Int
  , endByte :: {-# UNPACK #-}!Int
  , typ :: !Grammar
  , isMultiline :: !Bool -- TODO: maybe preserve line numbers instead so we can figure out which parts of a construct are multiline
  } deriving (Eq, Ord, Show)

abstract n =
  ANode
    { typ = toEnum (fromEnum (nodeSymbol n))
    , startByte = fromIntegral $ nodeStartByte n
    , endByte = fromIntegral $ nodeEndByte n
    , isMultiline = pointRow (nodeStartPoint n) /= pointRow (nodeEndPoint n)
    }

type TreeComp = RWS Inherited Synthesized Preceding

verbatim :: ANode -> TreeComp ()
verbatim n = do
  src <- asks source
  let bb = BB.byteString bs
      bs = BS.drop (startByte n) (BS.take (endByte n) src)
  tell mempty { out = bb }
  pure ()

formatter :: ANode -> [(ANode, TreeComp ())] -> TreeComp ()
formatter self children =
  case (typ self, map (\(node, comp) -> (typ node, comp)) children) of
            (Expression, _) -> do
              mconcat <$> traverse snd children
              -- pure $ mconcat (map snd children)

              -- TODO: This doesn't match comments. Not the end of the world, due
              --       to the verbatim fallback, but adding it in these pattern
              --       matches causes a combinatorial explosion.
              --
              --       What we may do instead is either
              --          - Ignore them here and make the write-side of TreeComp
              --            responsible for adding them back in.
              --          - Not use the built-in pattern matching but some
              --            sort of parser-like thing that remembers comments.
              --            Doing say *may* also make the match a bit more flexible,
              --            allowing more introspection without getting super verbose.
              --            *HOWEVER* we don't want to do any kind of deep matching
              --            because that means we enter O(n^2) territory.
              --            The Synthesized type provides memoization!
            (Function, [(Formals, formals), (AnonColon, colon), (_, body)]) -> do
              formals
              colon
              withOptionalIndent 2 $ do
                body

            (Function, [(Identifier, i), (AnonColon, colon), (Function, body)]) -> do
              i
              colon
              space
              body

            (Function, [(Identifier, i), (AnonColon, colon), (_, body)]) -> do
              i
              colon
              withOptionalIndent 2 $ do
                body

            (Formals, (AnonLBracket, open):children) -> do
              open
              void $ traverse snd children

            (Formal, [(Identifier, i)]) -> do
              i

            (Formal, [(Identifier, i), (AnonQuestion, q), (_, e)]) -> do
              i
              space
              q
              space
              e

            (App, [(_, a),(_, b)]) -> do
              a
              space
              b

            (Let, (Let, l):rest)
              | (bindings, rest') <- span (\(x, _) -> x == Bind || x == Inherit) rest
              , [(AnonIn, inkw), (bodyTyp, body) ] <- rest'
              -> do
              l
              withIndent 2 $
                void $ traverse snd bindings
              newline
              inkw
              if bodyTyp == Attrset
              then do
                space
                body
              else
                withIndent 2 $
                  body

            (Bind, [(Attrpath, a), (AnonEqual, eq1), (_, v), (AnonSemicolon, sc)]) -> do
              a
              space
              eq1
              space
              withIndent 2 $ do
                v
                sc
              newline

            (Inherit, [(Inherit, inhKw), (Parenthesized, p), (Attrs, attrs), (AnonSemicolon, sc)]) -> do
              inhKw
              space
              p
              withIndent 2 $ do
                space
                attrs
              sc
              newline

            (Inherit, [(Inherit, inhKw), (Attrs, attrs), (AnonSemicolon, sc)]) -> do -- FIXME specific attrs
              inhKw
              withIndent 2 $ do
                space
                attrs
              sc
              newline

            (Attrset, (AnonLBracket, open):rest)
              | (bindings, rest') <- span (\(x, _) -> x == Bind || x == Inherit) rest
              , [(AnonRBracket, close)] <- rest'
              -> do
              open
              withIndent 2 $
                void $ traverse snd bindings
              close

            (Attrs, as) | all (\(x, _) -> x == Identifier) as -> do
              void $ forM as $ \(_, i) -> do
                space
                i

            (AnonRBracket, []) -> verbatim self
            (AnonLBracket, []) -> verbatim self
            (AnonColon, []) -> verbatim self
            (Identifier, []) -> verbatim self
            (AnonEqual, []) -> verbatim self
            (AnonIn, []) -> verbatim self
            (Inherit, []) -> verbatim self
            (Let, []) -> verbatim self
            (AnonQuestion, []) -> verbatim self
            (AnonSemicolon, []) -> verbatim self
            (Spath, []) -> verbatim self
            (x, y) -> do
              tell mempty { fallbackNodes = S.singleton self }
              verbatim self

-- TODO: this currently breaks withIndent. The need for an output abstraction
--       is discussed in a comment at Synthesized.
newline :: TreeComp ()
newline = do
  ind <- asks indent
  tell mempty { out = "\n" <> BB.byteString (BS.replicate ind (charCode ' ')) }

-- TODO: replace by flexible space that can convert to newline
space :: TreeComp ()
space =
  tell mempty { out = " " }

withIndent :: Int -> TreeComp a -> TreeComp a
withIndent n m = do
  local (\inh -> inh { indent = indent inh + n }) (newline *> m)

-- | Indent except at top level
-- Does put output on new line
withOptionalIndent :: Int -> TreeComp a -> TreeComp a
withOptionalIndent n m = do
  tl <- asks indent
  if (tl == 0) then (newline *> m) else withIndent n m

charCode = fromIntegral . ord

data Inherited = Inherited
  { indent :: Int
  , source :: ByteString
  }
rootInherited :: ByteString -> Inherited
rootInherited bs = Inherited { indent = 0, source = bs }

-- TODO: Add support for ranges where formatting is disabled here in Preceding.
--       Probably as simple as a Bool here that is set and read as appropriate.
--       It can be closed explicitly like `/*nofmt*/ stuff /*dofmt*/`, or just
--       `/*nofmt*/ stuff`. Perhaps the implicit-looking approach is better here
--       because you can't forget to close it and with the explicit closing it
--       may be impossible to make the parent not interfere with a range that
--       consists of multitple nodes in that parent.
data Preceding = Preceding
  {}
rootPreceding :: Preceding
rootPreceding = Preceding {}

data Synthesized = Synthesized
  { unknownTypes :: Set String
  , fallbackNodes :: Set ANode
  , out :: Builder
    -- TODO:
    --
    -- Don't build the bytestring immediately but use something *slightly*
    -- fancier. We can avoid a full blown pretty printing library, because
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
    --    b = false;                   oneOrMoreLines;
    --  }                            b =
    --    potential extra   ------>    false;
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
  }
  deriving Generic
instance Semigroup Synthesized where (<>) = gmappend
instance Monoid Synthesized where { mappend = gmappend; mempty = gmempty }

-- Needs a newtype and is probably a bad idea anyway. We have no business
-- writing anything besides whitespace.
--instance IsString (TreeComp a) where
--  fromString s = tell mempty { out = fromString s }


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
  theType <- peekCString nodeType
  let TSPoint {..} = nodeStartPoint
      start        = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  let TSPoint {..} = nodeEndPoint
      end          = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  hPutStrLn stderr $ replicate ind ' ' ++ theType ++ start ++ "-" ++ end
  hPutStrLn stderr $ replicate ind ' ' ++ show (nodeInner source n)
