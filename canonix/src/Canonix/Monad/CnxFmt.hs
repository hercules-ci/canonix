{-# LANGUAGE DeriveGeneric #-}
module Canonix.Monad.CnxFmt 
  ( -- * CnxFmt monad
    CnxFmt
  , ErrorMessage
  , runFmt

    -- * Root-to-leaf
    --
    -- overridable, like Reader
  , Inherited(..)
  , rootInherited
  , remainingWidth
  , askParent
  , asksParent
  , tellChildren

    -- * Leaf-to-root
    --
    -- monoidal, overridable, like Writer
  , Synthesized(..)
  , tellParent
  , censorChildren

    -- * Start-to-end
  , Preceding
  , rootPreceding

    -- * Formatting functions

  , verbatim
  , space
  , newline
  , emptyLine
  , finalNewline

  , withIndent
  , withIndent'
  , withOptionalIndent
  
  , preserveEmptyLinesBefore
  , trySingleLine
  , forceMultiline

  ) where

import           Canonix.Monad.Fmt
import           Canonix.Space
import           Canonix.Node
import Control.Monad
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Monoid                    ( Ap(Ap) )
import           Data.Semigroup                 ( Max(Max) )
import           Data.Semigroup.Generic
import           Data.Set                       ( Set )
import           GHC.Generics                   ( Generic )

type CnxFmt
  = Fmt Inherited Synthesized Preceding (Piece (Indented LogicalSpace)) ErrorMessage

type ErrorMessage = String

-- | Formatting information that flows from the root of the expression to the
-- leaves.
data Inherited = Inherited
  { indent :: Int
  , source :: ByteString
  , singleLineAllowed :: Bool
  }
rootInherited :: ByteString -> Inherited
rootInherited bs =
  Inherited { indent = 0, source = bs, singleLineAllowed = False }

remainingWidth :: Inherited -> Int
remainingWidth inh = max 30 (78 - indent inh)



-- | Formatting information that flows from the leaves to the root of the
-- expression.
data Synthesized = Synthesized
  { unknownTypes :: Set String
  , fallbackNodes :: Set Node
  , singleLine :: Ap Maybe [Piece ()]
    -- ^
    -- singleLine represents a formatted single-line variation of the subtree, or
    -- Nothing when the line is too long, or if the original tree is multiline.
    --
    -- See 'trySingleLine'
  }
  deriving (Generic, Show)
instance Semigroup Synthesized where (<>) = gmappend
instance Monoid Synthesized where { mappend = gmappend; mempty = gmempty }



data Preceding = Preceding
  { furthestRow :: Int
  }
rootPreceding :: Preceding
rootPreceding = Preceding
  { furthestRow = 0
  }

-- | Max number of consecutive newlines to preserve
linePreservationCap :: Int
linePreservationCap = 3

preserveEmptyLinesBefore :: Node -> CnxFmt () -> CnxFmt ()
preserveEmptyLinesBefore n m = do
  fr <- furthestRow <$> askPreceding
  let skippedDistance = startRow n - fr

  when (skippedDistance > 1) $ do
    ind <- asksParent indent
    write $ SpaceRequest (pure ind, Linebreak $ Max $ min linePreservationCap (skippedDistance - 1))

  m

  tellSucceeding (\pre -> pre { furthestRow = max (furthestRow pre) (fromIntegral (endRow n)) })

-- | Copy a node to the output without any changes
verbatim :: Node -> CnxFmt ()
verbatim n = do
  src <- asksParent source
  let bs = BS.drop (startByte n) (BS.take (endByte n) src)
  write $ NonSpace bs
  _ <- whenSingleLineAllowed $ do
    rw <- asksParent remainingWidth
    tellParent mempty { singleLine = pure (NonSpace bs) <$ guard (BS.length bs <= rw) }
  pure ()

-- | Just a space
space :: CnxFmt ()
space = do
  write $ SpaceRequest $ pure Space
  void $ whenSingleLineAllowed $
    tellParent mempty { singleLine = pure [SpaceRequest ()] }

-- | A newline in multiline format, but a space in single line format
newline :: CnxFmt ()
newline = do
  ind <- asksParent indent
  write $ SpaceRequest (pure ind, Linebreak 0)
  void $ whenSingleLineAllowed $
    tellParent mempty { singleLine = pure [SpaceRequest ()] }

-- | An empty line in multiline format, but a space in single line format
emptyLine :: CnxFmt ()
emptyLine = do
  ind <- asksParent indent
  write $ SpaceRequest (pure ind, Linebreak 0)
  void $ whenSingleLineAllowed $
    tellParent mempty { singleLine = pure [SpaceRequest ()] }

-- | Request a line break and make the 'Canonix.Space' module write it out by
-- also inserting an empty 'NonSpace'.
finalNewline :: CnxFmt ()
finalNewline = write (SpaceRequest (pure 0, Linebreak 0)) >> write (NonSpace mempty)


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
        ln :: Ap Maybe [Piece ()]
        ln = do
          l0 <- singleLine syn
          l0 <$ guard (piecesLength l0 <= rw) -- TODO: unicode width

      (ln, a) <$ tellParent (syn { singleLine = ln })
      ) $ \multilineChunks (ln, a) -> do
      case ln of
        Ap (Just l) ->
          mapM_ (write . (pure Space <$)) l
        _ -> mapM_ write multilineChunks
      pure a


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
