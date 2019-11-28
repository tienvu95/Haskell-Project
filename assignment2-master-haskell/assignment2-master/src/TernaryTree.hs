module TernaryTree ( insert_prefix
                   , lookup_prefix
                   , get_ternaryTree_keys
                   , get_ternaryTree_mappings
                   , test1, test2
                   , TernaryTree
                   , make_initial_tree
                   ) where

import Data.Char (ord)
import Text.Printf (printf)
import Data.List (sort)

data TernaryTree a
    = Empty
    | Node a Int (TernaryTree a) (TernaryTree a) (TernaryTree a)
    deriving (Eq)

lookup_prefix :: Ord a => [a] -> TernaryTree a -> Maybe Int
lookup_prefix = undefined -- TODO

insert_prefix :: Ord a => [a] -> Int -> TernaryTree a -> TernaryTree a 
insert_prefix = undefined -- TODO

test1, test2 :: TernaryTree Char
test1 = Node 'a' 1 Empty Empty (Node 'c' 2 (Node 'b' 3 Empty Empty Empty) Empty (Node 'd' 4 Empty Empty Empty))
test2 = Node 'a' 1 Empty (Node 'X' 7 (Node 'C' 8 Empty Empty Empty) Empty (Node 'Y' 9 Empty Empty Empty)) (Node 'c' 2 (Node 'b' 3 Empty Empty Empty) Empty (Node 'd' 4 Empty Empty Empty))

get_ternaryTree_keys :: TernaryTree a -> [[a]]
get_ternaryTree_keys tree = map fst (get_ternaryTree_mappings tree)

get_ternaryTree_mappings :: TernaryTree a -> [([a], Int)]
get_ternaryTree_mappings tree = case tree of
    Empty -> []
    (Node x n l e h) -> get_ternaryTree_mappings l
                     ++ [([x],n)]
                     ++ map (\(xs,m) -> (x:xs,m)) (get_ternaryTree_mappings e)
                     ++ get_ternaryTree_mappings h

make_initial_tree :: (Ord a, Enum a) => (a -> Int) -> a -> a -> TernaryTree a
make_initial_tree f a b = make_balanced_tree $ map (\x -> (x,f x)) [a..b]



instance Show a => Show (TernaryTree a) where
    show = unlines . show_ternary_tree
       
-- This code generously provided by Edward Kmett
show_ternary_tree :: Show a => TernaryTree a -> [String]
show_ternary_tree = go " " "+" " " where
  go _ m _ Empty = [m ++ "*"]
  go l m r (Node a n lt mt rt) =
       go (l ++ spaces) (l ++ node) (l ++ bar) lt
    ++ [l ++ bar]
    -- ++ go (l ++ bar) (m ++ "+ " ++ label ++ " --") (r ++ bar) mt
    ++ (if empty mt then [m ++ "+ " ++ label ++ " -*"]
                    else go (l ++ bar ++ "   ") (m ++ "+ " ++ label ++ " -") (r ++ bar ++ "   ") mt)
    ++ [r ++ bar]
    ++ go (r ++ bar) (r ++ node) (r ++ spaces) rt
    where ll = length label
          bar    = "   " ++ '|' : replicate (ll-3) ' '
          spaces = replicate (ll+1) ' '
          node   = "   " ++ '+' : replicate (ll-3) '-'
          label = printf "(%d) %s" n (show a)

empty :: TernaryTree a -> Bool
empty tree = case tree of
    Empty -> True
    _     -> False

make_balanced_tree :: Ord a => [(a,Int)] -> TernaryTree a
make_balanced_tree pairs = go (sort pairs) where
    go xs = case xs of
        [] -> Empty
        [(x,n)] -> Node x n Empty Empty Empty
        _ -> case splitAt (len `div` 2) xs of
            -- ([],(x,n):ys) -> Node x n Empty Empty (go ys)
            (ys,(x,n):zs) -> Node x n (go ys) Empty (go zs)
            _ -> Empty
        where len = length xs

{-
test:

prop_all_initial_prefixes_exist :: Ord a => (a -> Int) -> [(a,Int)] -> Property
prop_all_initial_prefixes_exist xs = 
-}


