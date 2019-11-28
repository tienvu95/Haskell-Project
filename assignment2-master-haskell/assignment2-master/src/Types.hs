-- The interface to the module Types is written out
-- explicitly here, after the module name.
module Types
  ( Tree(Leaf, Node)
  , Bit(L, R)
  , HCode
  , Table
  ) where

import Data.Map

-- Trees to represent the relative frequencies of characters
-- and therefore the Huffman codes.
data Tree a
  = Leaf Int
         a
  | Node Int
         (Tree a)
         (Tree a)
    deriving (Eq)

-- The types of bits, Huffman codes and tables of Huffman codes.
data Bit
  = L
  | R
  deriving (Eq, Show)

type HCode = [Bit]

type Table a = Map a HCode
