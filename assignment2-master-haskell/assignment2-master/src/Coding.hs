-------------------------------------------------------------------------
--  
--         Coding.hs                            
--                              
--         Huffman coding in Haskell.                   
--
--         The top-level functions for coding and decoding.
--
-------------------------------------------------------------------------
module Coding
  ( encodeMessage
  , decodeMessage
  ) where

import Types (Tree(Leaf, Node), Bit(L, R), HCode, Table)

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import CodeTable
import MakeTree
import Frequency


-- Code a message according to a table of codes, look up each character in the table, and concatenate the results.
encodeMessage
  :: Ord a
  => Table a -> [a] -> HCode
encodeMessage table = concat . map (lookup table)

-- lookup looks up the meaning of an individual char in a Table.
lookup
  :: Ord a
  => Table a -> a -> HCode
lookup m c = fromMaybe (error "lookup") (Map.lookup c m)

-- Decode a message according to a tree.                
--                              
-- The first tree argument is constant, being the tree of codes;
-- the second represents the current position in the tree relative  
-- to the (partial) HCode read so far.               
decodeMessage
  :: Ord a
  => Tree a -> HCode -> [a]
decodeMessage t b = decode t b
    where
        decode (Node n t1 t2) (L:list) = decode t1 list
        decode (Node n t1 t2) (R:list) = decode t2 list
        decode (Leaf n c) list = c : decode t list
        decode t [] = []

-------------------test---------------------
--test whether if the program encodes a certain message, when we decompress or decode the message, we get exactly the
--same thing back

--Create encondetest, I use codeTable, makeTree and frequency to convert the string "test whether it is wrong"
--to a table
encodetest:: HCode
encodetest = encodeMessage table "test whether this is working"
       where table = codeTable tree
             tree  = (makeTree (frequency "test whether this is working"))

--here is the result
--[R,L,R,L,R,L,L,L,R,R,L,R,R,L,L,R,R,L,R,L,R,R,L,R,L,R,L,R,L,R,R,L,R,L,R,R,L,L,R,L,L,R,L,R,L,R,R,L,L,L,L,L,R,R,L,L,
-- L,L,L,L,L,R,R,L,L,R,R,L,R,R,R,R,L,R,R,R,L,L,R,R,R,R,R,L,L,L,R,R,R,L,L,R,R,R,R,L]

--then I create a function test to check whether when I decode the result above, I will get the string
--"test whether this is working", if test returns true, it means the compression is lossless
test::Bool
test = decodeMessage tree encodetest == "test whether this is working"
       where tree  = (makeTree (frequency "test whether this is working"))
