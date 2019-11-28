module BWT where

import Data.Binary
import Data.List

import RLE

data BWTValue a
  = EOL -- End of line
  | Value a -- Wraps a character from the original string.
  deriving (Ord, Eq)

-- Transform a string into a string with the `special end character'
mark_end :: [a] -> [BWTValue a]
mark_end = undefined -- TODO

-- Produce all rotations of the input string.
-- Hint: look at the functions `inits` and `tails` and zipWith
make_rotations :: [BWTValue a] -> [[BWTValue a]]
make_rotations = undefined -- TODO

-- Sort the list of rotated 
sort_rotations
  :: Ord a
  => [[BWTValue a]] -> [[BWTValue a]]
sort_rotations = undefined -- TODO

-- Retrive the last BWTValue a from each of the sorted rotations
get_lasts :: [[BWTValue a]] -> [BWTValue a]
get_lasts = undefined -- TODO

-- Join it all together
bwt_encode
  :: Ord a
  => [a] -> [BWTValue a]
bwt_encode = undefined -- TODO

make_bwt_table
  :: Ord a
  => Int -> [BWTValue a] -> [[BWTValue a]]
make_bwt_table  = undefined -- TODO

remove_BWTValues :: [BWTValue a] -> [a]
remove_BWTValues = undefined -- TODO

find_EOL_line :: [[BWTValue a]] -> [BWTValue a]
find_EOL_line = undefined -- TODO

-- Decode BWT
bwt_decode
  :: Ord a
  => [BWTValue a] -> [a]
bwt_decode = undefined -- TODO

instance Show a =>
         Show (BWTValue a) where
  show EOL = "@"
  show (Value x) = show x

instance Binary a =>
         Binary (BWTValue a) where
  put EOL = putWord8 0
  put (Value x) = putWord8 1 >> put x
  get = do
    n <- getWord8
    case n of
      0 -> return EOL
      1 -> fmap Value get
      _ -> error $ "get(BWTValue a): unexpected tag: " ++ show n

instance Functor BWTValue where
  fmap _ EOL = EOL
  fmap f (Value x) = Value (f x)
