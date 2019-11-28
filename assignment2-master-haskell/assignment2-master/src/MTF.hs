module MTF where

import Data.List (elemIndex, delete)


mtf_encode :: Eq a => [a] -> [a] -> [Int]
mtf_encode = undefined -- TODO

mtf_decode :: Eq a => [a] -> [Int] -> [a]
mtf_decode = undefined -- TODO

safeIndex :: Int -> [a] -> Maybe a
safeIndex n _      | n < 0 = Nothing
safeIndex 0 (x:xs) = Just x
safeIndex n (_:xs) = safeIndex (n-1) xs
safeIndex _ []     = Nothing
