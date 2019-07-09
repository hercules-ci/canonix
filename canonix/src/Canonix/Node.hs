module Canonix.Node
 (
   Node(..)
)
where

import           TreeSitter.Nix

-- | "Abstract" node. Basically a tree-sitter Node, but pure Haskell and
-- excluding the children. Those are provided externally.
data Node = Node
  { startByte :: {-# UNPACK #-}!Int
  , endByte :: {-# UNPACK #-}!Int
  , typ :: !Grammar
  , isMultiline :: !Bool -- TODO: maybe preserve line numbers instead so we can figure out which parts of a construct are multiline
  } deriving (Eq, Ord, Show)
