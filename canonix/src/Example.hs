{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Example where

import qualified Data.ByteString               as BS
import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.Nix
import           TreeSitter.Node
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr                    ( Ptr(..)
                                                , nullPtr
                                                , plusPtr
                                                )
import           Foreign.Marshal.Alloc          ( malloc
                                                , mallocBytes
                                                )
import           Foreign.Marshal.Array          ( mallocArray )
import           Foreign.Storable               ( peek
                                                , peekElemOff
                                                , poke
                                                )
import           Foreign.Marshal.Utils          ( new )

import           Control.Monad
import           System.IO.Unsafe

import           TreeSitter.Ptr

import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.Node

import           Foreign.ForeignPtr
import           Foreign.C
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr                    ( Ptr(..)
                                                , nullPtr
                                                )

main :: IO ()
main = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_nix

  let source =
          "{ pkgs /* packages */, lib /* library  */ , ... }:\n{\n  fileSystems.\"/\".device = \"/dev/null\";\n}"
  BS.useAsCStringLen source $ \(str, len) -> do

    tree       <- ts_parser_parse_string parser nullPtr str len

    n          <- malloc
    ts_tree_root_node_p tree n

    n@Node {..} <- peek n
    let childCount = fromIntegral nodeChildCount

    children <- mallocArray childCount
    tsNode   <- malloc
    poke tsNode nodeTSNode
    ts_node_copy_child_nodes tsNode children

    let go n = 
          forChildren n $ \c -> do
            printNode source c
            go c
            pure ()
    go n

    pure ()

forChildren :: Node -> (Node -> IO a) -> IO [a]
forChildren n f = do
  let childCount = fromIntegral $ nodeChildCount n

  children <- mallocArray childCount
  tsNode   <- malloc
  poke tsNode (nodeTSNode n)
  ts_node_copy_child_nodes tsNode children

  forM [0 .. childCount - 1] $ \n -> do
    child <- peekElemOff children n
    f child

nodeInner :: BS.ByteString -> Node -> BS.ByteString
nodeInner bs n =
   BS.drop (fromIntegral $ nodeStartByte n)
   $ BS.take (fromIntegral $ nodeEndByte n) bs

printNode :: BS.ByteString -> Node -> IO ()
printNode source n@Node {..} = do
  theType <- peekCString nodeType
  let TSPoint {..} = nodeStartPoint
      start        = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  let TSPoint {..} = nodeEndPoint
      end          = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  putStrLn $ theType ++ start ++ "-" ++ end
  putStrLn $ show $ nodeInner source n
