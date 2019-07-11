module Canonix.Node
  ( Node(..)
  , isMultiline
  )
where

import           TreeSitter.Nix

-- | "Abstract" node. Basically a tree-sitter Node, but pure Haskell and
-- excluding the children. Those are provided externally.
data Node = Node
  { startByte :: {-# UNPACK #-}!Int
  , endByte :: {-# UNPACK #-}!Int
  , startRow :: {-# UNPACK #-}!Int
  , endRow :: {-# UNPACK #-}!Int
  , typ :: !Grammar
  } deriving (Eq, Ord, Show)

isMultiline :: Node -> Bool
isMultiline n = startRow n /= endRow n
