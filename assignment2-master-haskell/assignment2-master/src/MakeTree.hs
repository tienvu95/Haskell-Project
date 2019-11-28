-------------------------------------------------------------------------
--                              
--         MakeTree.hs                          
--                              
--         Turn a frequency table into a Huffman tree
--                              
--         (c) Addison-Wesley, 1996-2011.                   
--                          
-------------------------------------------------------------------------
module MakeTree
  ( makeTree
  ) where

import Types (Tree(Leaf, Node), Bit(L, R), HCode, Table)
import Frequency (frequency)

-- Some imports used in code provided to you, you can ignore them.
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.List (sortBy)
import Data.Ord  (comparing)
import Data.Char (isDigit)


--comparing :: Ord a => (b -> a) -> b -> b -> Ordering
--sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- Convert the trees to a list, then amalgamate into a single tree.
makeTree
  :: Ord a
  => [(a, Int)] -> Tree a
makeTree = makeCodes . toTreeList

-- Huffman codes are created bottom up: look for the least two frequent
-- letters, make these a new "isAlpha" (i.e. tree) and repeat until one tree
-- formed.  The function toTreeList makes the initial data structure.

--I create this function after reading and understanding
-- Haskell, The Craft of Functional Programming (pg 295 – 297) by Simon Thompson
toTreeList
  :: Ord a
  => [(a, Int)] -> [Tree a]
toTreeList = map (uncurry (flip Leaf))

-- The value of a tree.
value :: Tree a -> Int
value tree = case tree of
   (Leaf n _) -> n
   (Node n _ _) -> n

-- Merge two trees.                          
merge :: Tree a -> Tree a -> Tree a
merge tree1 tree2 = Node (value tree1 + value tree2) tree1 tree2

-- Sort a list of frequency trees by value (in ascending order)
sort :: [Tree a] -> [Tree a]
sort = sortBy (comparing value)

-- Merge the pair of trees at the front of the list
mergeFirstPair :: [Tree a] -> [Tree a]
mergeFirstPair (t1:t2:ts) = sort ((merge t3 t4):ts2)
        where (t3:t4:ts2) = sort (t1:t2:ts)


-- Make codes: amalgamate the whole list.               
makeCodes :: [Tree a] -> Tree a
makeCodes [tree] = tree
makeCodes list = makeCodes (mergeFirstPair list )


------------------------------------ Tests -----------------------------------
test :: Bool
test =
  sort (toTreeList $ frequency "Hello") ==
  sort [Leaf 1 'H', Leaf 1 'e', Leaf 2 'l', Leaf 1 'o'] &&
  sort
    (toTreeList $
     frequency "The quick brown fox jumped over the lazy dog.") ==
  sort
    [ Leaf 1 '.'
    , Leaf 1 'T'
    , Leaf 1 'a'
    , Leaf 1 'b'
    , Leaf 1 'c'
    , Leaf 1 'f'
    , Leaf 1 'g'
    , Leaf 1 'i'
    , Leaf 1 'j'
    , Leaf 1 'k'
    , Leaf 1 'l'
    , Leaf 1 'm'
    , Leaf 1 'n'
    , Leaf 1 'p'
    , Leaf 1 'q'
    , Leaf 1 't'
    , Leaf 1 'v'
    , Leaf 1 'w'
    , Leaf 1 'x'
    , Leaf 1 'y'
    , Leaf 1 'z'
    , Leaf 2 'd'
    , Leaf 2 'h'
    , Leaf 2 'r'
    , Leaf 2 'u'
    , Leaf 4 'e'
    , Leaf 4 'o'
    , Leaf 8 ' '
    ]
  where
    sort = sortBy (comparing value)

-- Make trees print out in a pretty way
instance Show a => Show (Tree a) where
    show (Leaf n x) = let xstr = show x in pad (length xstr) ' ' (show n) ++ "\n" ++ xstr
    show (Node n l r) = let
        llines = lines (show l)
        lmax   = maximum (map length llines)
        rlines = lines (show r)
        rmax   = maximum (map length rlines)
        nstr   = show n
        nwidth = length nstr
        depth  = length llines `max` length rlines
        spaces n     = replicate n ' '
        joined = zipWith3 (\left mid right -> left ++ mid ++ right)
                    (llines ++ repeat (spaces lmax))
                    (replicate depth (spaces nwidth))
                    (rlines ++ repeat (spaces rmax))
        
        nLine = spaces lmax ++ nstr ++ spaces rmax
        (barLine1, rest1) = break isDigit (head joined) -- find first digit
        (barLine2, rest2) = break (not . isDigit) rest1 -- skip digits
        (barLine3, rest3) = break isDigit rest2         -- find next digit
        (barLine4       ) = map (const ' ') rest3       -- fill the rest with spaces
        barLine = barLine1 ++ " " ++
                  "/" ++
                  map (const '-') (drop 2 $ barLine2 ++ barLine3) ++
                  "\\" ++
                  drop 1 barLine4
          in unlines (nLine:barLine:joined)

pad :: Int -> Char -> String -> String
pad width char str =
    let len = length str
        lenHalf = (width - len) `div` 2
    in replicate (width - len - lenHalf) char ++ str ++ replicate lenHalf char
 
instance Binary a => Binary (Tree a) where
    put (Leaf n x) = putWord8 0 >> putWord32be (fromIntegral n) >> put x
    put (Node _ l r) = putWord8 1 >> put l >> put r
    get = do
        b <- getWord8
        case b of
            0 -> do
                n <- getWord32be
                x <- get
                return $ Leaf (fromIntegral n) x
            1 -> do
                l <- get
                r <- get
                return $ merge l r

instance Functor Tree where
    fmap f (Leaf n x) = Leaf n (f x)
    fmap f (Node n l r) = Node n (fmap f l) (fmap f r)
