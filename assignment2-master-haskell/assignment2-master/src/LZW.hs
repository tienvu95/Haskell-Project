module LZW where

import TernaryTree

import qualified Data.Map as M
import Data.Char

data LZWState a = LZWState { lzwNextCode :: Int, lzwKnownPrefixes :: TernaryTree a }
    deriving Show

make_lzw_state :: TernaryTree a -> LZWState a
make_lzw_state tree = LZWState ((+1) . maximum . map snd . get_ternaryTree_mappings $ tree) tree

lzw_state_machine :: Ord a => [a] -> [a] -> LZWState a -> (LZWState a, Maybe Int,[a])
lzw_state_machine = undefined -- TODO
         
lzw_loop :: Ord a => [a] -> LZWState a -> [Int]
lzw_loop = undefined -- TODO

lzw_compress :: Ord a => TernaryTree a -> [a] -> [Int]
lzw_compress = undefined -- TODO

data LZWDecodeState a = LZWDecodeState Int (M.Map Int [a]) deriving Show

make_lzw_decode_state :: Ord a => TernaryTree a -> LZWDecodeState a
make_lzw_decode_state tree =
    LZWDecodeState 
        ((+1) . maximum . map snd . get_ternaryTree_mappings $ tree)
        (M.fromList . map (\(x,n) -> (n,x)) . get_ternaryTree_mappings $ tree)

lzw_decompress_state_machine :: LZWDecodeState a -> [a] -> Int -> (LZWDecodeState a, [a])
lzw_decompress_state_machine state prev input =
    let new_dict  = M.insert n (prev ++ take 1 entry) mp
        new_state = LZWDecodeState (n+1) new_dict
    in  (new_state, entry)
    where LZWDecodeState n mp = state
          entry = case M.lookup input mp of
            Just str -> str
            Nothing | input == n -> prev ++ take 1 prev
                    | otherwise  -> error $ "lzw_decompress_state_machine: unknown code: " ++ show input

lzw_decompress_loop :: LZWDecodeState a -> [a] -> [Int] -> [a]
lzw_decompress_loop state prev coded = case coded of
    [] -> []
    (x:xs) -> case lzw_decompress_state_machine state prev x of
        (_,[]) -> []
        (new_state,str) -> str ++ lzw_decompress_loop new_state str xs


lzw_decompress_loop' :: LZWDecodeState a -> [a] -> [Int] -> [[a]]
lzw_decompress_loop' state prev coded = case coded of
    [] -> []
    (x:xs) -> case lzw_decompress_state_machine state prev x of
        (_,[]) -> []
        (new_state,str) -> str : lzw_decompress_loop' new_state str xs
      

lzw_decompress :: Ord a => TernaryTree a -> [Int] -> [a]
lzw_decompress tree encoded = let state@(LZWDecodeState _ mp) = make_lzw_decode_state tree
    in case encoded of
        [] -> []
        (x:xs) -> let Just first = (M.lookup x mp)
                  in  first ++ lzw_decompress_loop state first xs

lzw_decompress' :: Ord a => TernaryTree a -> [Int] -> [[a]]
lzw_decompress' tree encoded = let state@(LZWDecodeState _ mp) = make_lzw_decode_state tree
    in case encoded of
        [] -> []
        (x:xs) -> let Just first = (M.lookup x mp)
                  in  first : lzw_decompress_loop' state first xs

test_lzw_identity :: Ord a => TernaryTree a -> [a] -> Bool
test_lzw_identity tree xs = xs == lzw_decompress tree (lzw_compress tree xs)


example_test = lzw_compress az_tree "aaaabaaab"
             == [0,26,0,1,27,1]

test_string = "tobeornottobeortobeornot"
simple_test = lzw_compress az_tree test_string
            ==  [19,14,1,4,14,17,13,14,19,26,28,30,35,29,31,33]


az_tree, ascii_tree :: TernaryTree Char
az_tree = make_initial_tree (\x -> ord x - ord 'a') 'a' 'z'
ascii_tree = make_initial_tree ord '\0' '\255'

