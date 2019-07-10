{-# LANGUAGE RecordWildCards #-}
module Canonix.TreeSitter
  ( mkTree
  , dump
  , forChildren
  , abstract
  )
where

import           Control.Comonad.Cofree
import           Control.Monad
import qualified Data.ByteString               as BS
import           Foreign.Marshal.Array          ( peekArray
                                                , allocaArray
                                                )
import           Foreign.Marshal.Utils          ( with )
import           System.IO
import           TreeSitter.Nix
import           TreeSitter.Node         hiding ( Node )
import qualified TreeSitter.Node               as TS
import           Canonix.Node

-- Cofree [] Node: rose tree (n-ary tree) with Node as the label for each node
mkTree :: TS.Node -> IO (Cofree [] TS.Node)
mkTree n = (n :<) <$> mkChildren
  where mkChildren = forChildren n $ \c -> mkTree c

forChildren :: TS.Node -> (TS.Node -> IO a) -> IO [a]
forChildren n f = do
  let count = fromIntegral $ nodeChildCount n
  children <- allocaArray count $ \childNodesPtr -> do
    _ <- with (nodeTSNode n) (`ts_node_copy_child_nodes` childNodesPtr)
    peekArray count childNodesPtr
  forM children f

nodeInner :: BS.ByteString -> TS.Node -> BS.ByteString
nodeInner bs n = BS.drop (fromIntegral $ nodeStartByte n)
  $ BS.take (fromIntegral $ nodeEndByte n) bs

printNode :: Int -> BS.ByteString -> TS.Node -> IO ()
printNode ind source n@TS.Node {..} = do
  let start =
        let TSPoint {..} = nodeStartPoint
        in  "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
      end =
        let TSPoint {..} = nodeEndPoint
        in  "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
      typ :: Grammar
      typ = toEnum $ fromEnum $ nodeSymbol
  hPutStrLn stderr $ replicate (ind * 2) ' ' ++ show typ ++ start ++ "-" ++ end
  hPutStrLn stderr $ replicate (ind * 2) ' ' ++ show (nodeInner source n)

dump
  :: BS.ByteString -- ^ Source file contents
  -> Int
  -> TS.Node -- ^ Must come from the same source
  -> IO ()
dump source i nd = do
  printNode i source nd
  void $ forChildren nd $ \c -> dump source (i + 1) c

abstract :: TS.Node -> Node
abstract n = Node
  { typ         = toEnum (fromEnum (nodeSymbol n))
  , startByte   = fromIntegral $ nodeStartByte n
  , endByte     = fromIntegral $ nodeEndByte n
  , isMultiline = pointRow (nodeStartPoint n) /= pointRow (nodeEndPoint n)
  }
