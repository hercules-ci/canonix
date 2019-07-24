{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Formatting is split into two 'phases' that with an interface that
-- represents verbatim text, indentation and whitespace types
module Canonix.Space where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Char                      ( ord )
import           Data.Monoid                    ( Last )
import           Data.Semigroup                 ( Max(Max) )
import           Data.Word                      ( Word8 )
import           Pipes

data Piece indent ws
  = NonSpace !indent !ByteString
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
renderSpaces :: Monad m => Pipe (Piece Int LogicalSpace) ByteString m a
renderSpaces = start
 where
  -- Ignore space requests at file start
  start = await >>= \case
    SpaceRequest _ -> start
    NonSpace i bs -> writeNonSpace i Nothing bs

  go ws = await >>= \case
    NonSpace i bs -> writeNonSpace i ws bs
    SpaceRequest ws2 -> go (ws <> Just ws2)

  writeNonSpace i ws bs = do
    writeSpace i ws
    yield bs
    go Nothing

  writeSpace i = mapM_ $ \case
    Space -> yield " "
    Linebreak (Max n) ->
      yield (BS.replicate (n + 1) (charCode '\n')) >> writeInd i

  writeInd i = yield (BS.replicate i (charCode ' '))

piecesLength :: [Piece i ()] -> Int
piecesLength = f mempty
 where
  f ws (NonSpace    _ bs : pcs') = length ws + BS.length bs + f mempty pcs'
  f ws (SpaceRequest ws2 : pcs') = f (ws <> Just ws2) pcs'
  f _  []                        = 0

charCode :: Char -> Word8
charCode = fromIntegral . ord
