{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Formatting is split into two 'phases' that with an interface that
-- represents verbatim text, indentation and whitespace types
module Canonix.Space where

import           Control.Monad
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Char                      ( ord )
import           Data.Monoid                    ( Last )
import           Data.Semigroup                 ( Max(Max) )
import           Data.Word                      ( Word8 )
import           Pipes

data Piece ws
  = NonSpace !ByteString
  | SpaceRequest !ws
  deriving (Show, Functor)

type Indented a = (Last Int, a)

data LogicalSpace
  = Space
  | Linebreak (Max Int) -- ^ @Linebreak 0@ is a line break, @Linebreak 1@ is a single empty line and so on.
  deriving Show

-- | 'Space' loses to 'Linebreak'. Greatest number of empty lines wins.
instance Semigroup LogicalSpace where
  Space       <> x           = x
  x           <> Space       = x
  Linebreak x <> Linebreak y = Linebreak (x <> y)

-- | Note: Does not print whitespace at the end. Insert a @'Verbatim' ""@ to write the final whitespace.
--
-- The representation of indentation is probably not ideal but seems sufficient.
renderSpaces :: Monad m => Pipe (Piece (Indented LogicalSpace)) ByteString m a
renderSpaces = go Nothing
 where
  go ws = await >>= \case
    NonSpace bs -> do
      writeSpace ws
      yield bs
      go Nothing
    SpaceRequest ws2 -> go (ws <> Just ws2)

  writeSpace = mapM_ $ \case
    (_, Space) -> yield " "
    (ind, Linebreak (Max n)) ->
      yield (BS.replicate (n + 1) (charCode '\n')) >> writeInd ind

  writeInd ind = forM_ ind $ \i -> yield (BS.replicate i (charCode ' '))

piecesLength :: [Piece ()] -> Int
piecesLength = f mempty
 where
  f ws (NonSpace     bs  : pcs') = length ws + BS.length bs + f mempty pcs'
  f ws (SpaceRequest ws2 : pcs') = f (ws <> Just ws2) pcs'
  f _  []                        = 0

charCode :: Char -> Word8
charCode = fromIntegral . ord
