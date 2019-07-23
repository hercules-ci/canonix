{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Format where

import           Canonix.Node
import           Canonix.Space
import           Canonix.TreeSitter
import           Canonix.Monad.CnxFmt
import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as T
import qualified Control.Exception
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as BL
import           Data.Foldable
import           Data.Functor.Foldable
import qualified Data.Set                      as S
import           Foreign.Marshal.Alloc          ( malloc )
import           Foreign.Ptr                    ( nullPtr )
import           Foreign.Storable               ( peek )
import           Pipes
import qualified Pipes.Lift
import           System.IO
import           TreeSitter.Nix
import           TreeSitter.Parser
import           TreeSitter.Tree
-- import Debug.Trace (trace)

traceDemand :: String -> a -> a
traceDemand = flip const  -- folds away the debug statements
-- traceDemand = trace  -- should show an interleaving of Chunk production and Node demand

format :: Bool -> FilePath -> ByteString -> IO BL.ByteString
format debug filepath src = do
  -- TODO: bracket patterns and move to Canonix.TreeSitter
  parser <- ts_parser_new
  _ <- ts_parser_set_language parser tree_sitter_nix

  BS.useAsCStringLen src $ \(str, len) -> do

    ttree       <- ts_parser_parse_string parser nullPtr str len

    n          <- malloc
    ts_tree_root_node_p ttree n

    root <- peek n

    when debug $ dump src 0 root

    tree <- mkTree root
    let
      (_ast, cnxfmt) = walk (fmap (lg . abstract) tree) formatter
      lg nd = traceDemand ("Demanding " <> show nd) nd

      produce = do
        r <- Pipes.hoist (pure . runIdentity) (runFmt (cnxfmt >> finalNewline) (rootInherited src) rootPreceding) >-> renderSpaces >-> forever (await >>= (yield . Left))
        yield (Right r)
        liftIO $ Control.Exception.throwIO $ Control.Exception.AssertionFailed "Can't consume past end"

      consume = do
        (r, x) <- Pipes.Lift.runStateP (mempty :: BB.Builder) $
          fix $ \nxt ->
            await >>= \case
              Left chunk0 -> do

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
                modify' (<> BB.byteString chunk)
                nxt
              Right end ->
                pure end

        (synthesized, _lastPreceding, _a) <- case r of
          Left e -> liftIO $ Control.Exception.throwIO $ Control.Exception.ErrorCall $ renderError filepath e
          Right r' -> pure r'

        when debug $ lift $ do
          hPutStrLn stderr $ "Encountered these unknown node types: " <> show (unknownTypes synthesized)
          hPutStrLn stderr $ "Verbatim fallback nodes: " <> show (fallbackNodes synthesized)

        pure $ BB.toLazyByteString x


    Pipes.runEffect (produce >-> consume)

renderError :: FilePath -> Error -> String
renderError fp er = fp <> ":" <> show (1 + startRow (errorLocation er)) <> ": " <> errorMessage er


-- | Traverse a tree, invoking the provided function at each node.
--
-- The function is responsible for visiting the children that are passed in.
--
-- The original values for the children made available directly to the function.
walk :: Cofree [] Node -> (Node -> [(Node, a)] -> a) -> (Node, a)
walk t f = cataA (\(self T.:< c) -> (self, f self c)) t

-- | Tell the children what they need to know from their parent.
withSelf :: Node -> CnxFmt () -> CnxFmt ()
withSelf self fmt = do
  inh <- askParent
  tellChildren inh { singleLineAllowed = not (isMultiline self) } fmt

matchOneOfTypeWithComments :: Applicative m => [(Node, m a)] -> Maybe (Grammar, m a, [(Node, m a)])
matchOneOfTypeWithComments = go id
  where
    go acc ((n, a) : as) | typ n == Comment = go (acc . (a *>)) as
    go acc ((t, a) : as) = Just (typ t, (acc a), as)
    go _ [] = Nothing

matchOneWithComments :: Applicative m => (Node -> Bool) -> [(Node, m a)] -> Maybe (Grammar, m a, [(Node, m a)])
matchOneWithComments p = go id
  where
    go acc ((n, a) : as) | p n = Just (typ n, (acc a), as)
    go acc ((n, a) : as) | typ n == Comment = go (acc . (a *>)) as
    go _ _ = Nothing

matchManyOfTypeWithComments :: Applicative m => (Node -> Bool) -> [(Node, m a)] -> ([(Grammar, m a)], [(Node, m a)])
matchManyOfTypeWithComments p = go id
  where
    go acc l = case matchOneWithComments p l of
                      Just (g, m, rest) -> go (acc . ((g, m):)) rest
                      Nothing -> (acc [], l)

pattern One :: Applicative m => Grammar -> m a -> [(Node, m a)] -> [(Node, m a)]
pattern One t m rest <- (matchOneOfTypeWithComments -> Just (t, m, rest))

spanTypes :: Applicative m => [Grammar] -> [(Node, m a)] -> ([(Grammar, m a)], [(Node, m a)])
spanTypes ts = matchManyOfTypeWithComments (\x -> typ x `elem` ts)

pattern Comments :: Applicative m => [(Grammar, m a)] -> [(Node, m a)] -> [(Node, m a)]
pattern Comments cs rest <- (spanTypes [Comment] -> (cs, rest))

pattern (:*:) :: a -> b -> (a, b)
pattern a :*: b = (a, b)

formatter :: Node -> [(Node, CnxFmt ())] -> CnxFmt ()
formatter self children = withSelf self $ preserveEmptyLinesBefore self $ trySingleLine $ do
  sl <- asksParent singleLineAllowed
  case (typ self, children) of
    -- Expression is the root node of a file
    (Expression, _) -> do
      mconcat <$> traverse snd children
      newline

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
      One AnonLet l
        (spanTypes [Bind, Inherit] -> bindings :*:
          (One AnonIn inkw
            (One bodyTyp body [])
          )
        )
      ) -> do
      l
      withIndent 2 $ forM_ bindings $ \(_, b) -> do
        b
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

    (Parenthesized,
      One AnonLParen open
        (matchManyOfTypeWithComments ((/=) AnonRParen . typ) -> bindings :*:
          Comments finalComments (
            One AnonRParen close []
          )
        )
      ) -> do
        open
        withIndent 2 $ do
          forM_ bindings $ \(_, i) -> do
            newline
            i
          mapM_ snd finalComments
        newline
        close

    (List, [(ndOpen, open), (ndClose, close)])
      | sl && typ ndOpen == AnonLBracket && typ ndClose == AnonRBracket  -> do
      open
      close

    (List,
      One AnonLBracket open
        (matchManyOfTypeWithComments ((/=) AnonRBracket . typ) -> bindings :*:
          Comments finalComments (
            One AnonRBracket close []
          )
        )
      ) -> do
        open
        withIndent 2 $ do
          forM_ bindings $ \(_, i) -> do
            newline
            i
          mapM_ snd finalComments
        newline
        close

    (Attrset, [(ndOpen, open), (ndClose, close)])
      | sl && typ ndOpen == AnonLBrace && typ ndClose == AnonRBrace  -> do
      open
      close

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

    (With, One AnonWith with (One _ a (One AnonSemicolon semicol (One _ sub [])))) -> do
      with
      space
      withIndent' 2 a
      semicol
      emptyLine
      sub

    (AnonRBracket, []) -> verbatim self
    (AnonLBracket, []) -> verbatim self
    (AnonRBrace, []) -> verbatim self
    (AnonLBrace, []) -> verbatim self
    (AnonRParen, []) -> verbatim self
    (AnonLParen, []) -> verbatim self
    (AnonColon, []) -> verbatim self
    (Identifier, []) -> verbatim self
    (AnonEqual, []) -> verbatim self
    (AnonIn, []) -> verbatim self
    (AnonWith, []) -> verbatim self
    (AnonLet, []) -> verbatim self
    (Inherit, []) -> verbatim self
    (Let, []) -> verbatim self
    (AnonQuestion, []) -> verbatim self
    (AnonSemicolon, []) -> verbatim self
    (Spath, []) -> verbatim self
    (Integer, []) -> verbatim self
    (ParseError, _) -> throw Error { errorLocation = self, errorMessage = "parse error" }
    (Comment, []) -> do
      forceMultiline
      newline -- TODO: clearline
      verbatim self
      newline
    (_x, _y) -> do
      tellParent mempty { fallbackNodes = S.singleton self }
      verbatim self
