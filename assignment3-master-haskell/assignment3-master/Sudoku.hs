-- Name: Tien Vu Hoang
-- UID: u5846163
-- Collaborators: Jane Doe, Joe Bloggs
module Sudoku
  ( allBlanks
  , isSudoku
  , noBlanks
  , printSudoku
  , fromString
  , toString
  , rows
  , cols
  , boxs
  , okBlock
  , okSudoku
  , blank
  , (!!=)
  , update
  , solve
  , solve2
  ) where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List


-- A matrix is a list of rows.
type Matrix a = [Row a]

-- A row is a list of values
type Row a = [a]

-- A Sudoku puzzle is a matrix of cells
newtype Sudoku =
  Sudoku (Matrix Cell)
  deriving (Show, Eq)

-- | cells extracts the cells from a Sudoku
cells (Sudoku m) = m

-- Each cell may contain a number from 1 to 9, or nothing
type Cell = Maybe Int

example :: Sudoku
example =
  Sudoku
    [ [ Just 3
      , Just 6
      , Nothing
      , Nothing
      , Just 7
      , Just 1
      , Just 2
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Just 5
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 1
      , Just 8
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 9
      , Just 2
      , Nothing
      , Just 4
      , Just 7
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 1
      , Just 3
      , Nothing
      , Just 2
      , Just 8
      ]
    , [ Just 4
      , Nothing
      , Nothing
      , Just 5
      , Nothing
      , Just 2
      , Nothing
      , Nothing
      , Just 9
      ]
    , [ Just 2
      , Just 7
      , Nothing
      , Just 4
      , Just 6
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 5
      , Just 3
      , Nothing
      , Just 8
      , Just 9
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Just 8
      , Just 3
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 6
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 7
      , Just 6
      , Just 9
      , Nothing
      , Nothing
      , Just 4
      , Just 3
      ]
    ]

-- allBlanks is a Sudoku with just blanks
allBlanks :: Sudoku
allBlanks = Sudoku (replicate 9 (replicate 9 Nothing))
-- replicate x9 of 9 Nothing

-- matrix a = [[a]] --> [[Maybe Int]]
-- | isSudoku checks if a Sudoku has the proper dimensions
-- >>> isSudoku (Sudoku [])
-- False
-- >>> isSudoku allBlanks
-- True
-- >>> isSudoku example
-- True
-- >>> isSudoku (Sudoku (tail (cells example)))
-- False
isSudoku :: Sudoku -> Bool
isSudoku sudo_check = let no_of_rows = cells sudo_check --extract the cells
                      in length no_of_rows == 9 --Has 9 Rows (count no. of list in the matrix)
                         && all ((== 9) . length) no_of_rows --Has 9 items in each row
                         -- ((== 9) . length) is the condition for every list in the matrix
                         && all (\ x -> isNothing x || fromJust x < 10 && fromJust x > 0) (concat no_of_rows)
                         -- concat makes lists of list become list ==> so that "all" function can run thru
                         -- check if every cell is either a blank (isNothing) or Just Int (1-9)
-------------------------------------
--test for isSudoku
prop_isSudoku :: Sudoku -> Bool
prop_isSudoku s = isSudoku s

----------------------

-- | noBlanks checks if a Sudoku has no blanks
noBlanks :: Sudoku -> Bool
noBlanks sudo_check = all isJust (concat (cells sudo_check))
-- check if all the cells are just numbers, no "Nothing" value in the cells
-- logic is simple, make sudoku become a list with 81 elements (cells)
-- run thru the list using 'all', check using 'isJust'

-- | cell generates an arbitrary cell in a Sudoku
-- The frequency of Nothing versus Just n values is currently 90% versus 10%,
-- but you may want to change that ratio.
cell :: Gen (Maybe Int)
cell =
  frequency
    [(10, oneof [return (Just n) | n <- [1 .. 9]]), (90, return Nothing)]

-- | An instance for generating Arbitrary Sudokus
-- prop> isSudoku s
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- sequence [sequence [cell | j <- [1 .. 9]] | i <- [1 .. 9]]
    return (Sudoku rows)

-- | printSudoku prints a Sudoku as a 9 x 9 grid
-- Example:
--    3 6 . . 7 1 2 . .
--    . 5 . . . . 1 8 .
--    . . 9 2 . 4 7 . .
--    . . . . 1 3 . 2 8
--    4 . . 5 . 2 . . 9
--    2 7 . 4 6 . . . .
--    . . 5 3 . 8 9 . .
--    . 8 3 . . . . 6 .
--    . . 7 6 9 . . 4 3
-- have to change sudoku --> string cause putStr input = String

-- | toString converts a Sudoku into its canonical 81-character string
-- | encoding
-- prop> fromString (toString s) == s
-- In my code, i do not use the given type of toString :: Sudoku -> String
toString :: Sudoku -> [String]
toString sudoku = map (map symbolChar) (cells sudoku)
    where symbolChar = maybe '.' intToDigit

-- | fromString converts an 81-character canonical string encoding for a
-- | Sudoku into our internal representation
listRows :: String -> [String]
listRows s = splitby9 9 s

-- convert all the elements in list, one by one
convert :: String -> [Maybe Int]
convert []                 = []
convert ('.':xs)           = [Nothing] ++ convert xs
convert ('0':xs)           = [Nothing] ++ convert xs -- <-- for sudoku17.txt file
convert (x:xs) | isDigit x = [Just (digitToInt x)] ++ convert xs
               | otherwise = error "Invalid sudoku, numbers only!"

--break list of cells into group of 9, so that when print, only 9 cells
-- will be displayed on one line
-- This function was created with the help of Data.List.Grouping Haskell package
-- https://hackage.haskell.org/package/list-grouping-0.1.1/src/Data/List/Grouping.hs
splitby9 :: Int -> [a] -> [[a]]
splitby9 _ [] = []
splitby9 n xs = l1 : splitby9 n l2
  where (l1,l2) = splitAt n xs

-- convert string to [[Maybe Int]] = sudoku
fromString :: String -> Sudoku
fromString xs = Sudoku ([convert s | s <- ys])
       where ys = listRows xs

--- use unlines to convert [String] to String
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = putStr (unlines $ toString sudoku)



-----------------------------------
{- just like definition of type Row
here is the functions with given types, it works but lengthy, so I prefer
the below version

type Block a = [a]

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Block a]
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> [Block a]
boxs =  map concat . groupBy3 . concat . transpose . map groupBy3

groupBy3 :: [a] -> [[a]]
groupBy3 (a:b:c:ds) = [a,b,c] : groupBy3 ds
groupBy3 []         = []

okBlock []           = True
okBlock (Nothing:xs) = okBlock xs
okBlock (x:xs)       = (notElem x xs) && (okBlock xs)

okSudoku :: Sudoku -> Bool
okSudoku m = all nodups [rows (cells m)] &&
             all nodups [cols (cells m)] &&
             all nodups [boxs (cells m)]

nodups :: Eq a => [Block a] -> Bool
nodups []     = True
nodups (x:xs) = (notElem x xs) && (nodups xs)
-}
--Check that a block contains no duplicate values
-----------------------------------



type Block = [Maybe Int]
-- a block can be row, a column or a 3x3 box

-- | Test if a block of cells does not contain the same integer twice
-- >>> okBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
-- True
-- >>> okBlock [Just 1, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]
-- False


okBlock :: Block -> Bool
okBlock []           = True
okBlock (Nothing:xs) = okBlock xs
okBlock (x:xs)       = (notElem x xs) && (okBlock xs)

-- Extracts all rows, columns & boxes
things_to_check :: Sudoku -> [Block]
things_to_check s = (cells s) ++ (transpose (cells s)) ++ boxes s where
    boxes s = [boxs (x,y) s | y <- [0..2], x <- [0..2]]
------------
-- test for things_to_check function, which combines 9 rows, 9 columns and 9 boxes
-- Thus, check if there are 27 elements (lists) and if each list has 9 elements
-- either nothing or Just Int

prop_things_to_check :: Sudoku -> Bool
prop_things_to_check s = ((length bl) == 3*9) &&
                and [(length b) == 9 | b <- bl]
  where bl = things_to_check s
-------
-- Get column x from sudoku s
cols :: Int -> Sudoku -> Block
cols x s = transpose (cells s) !! x

-- Get row x from sudoku s, use !! to locate the position of row,
rows :: Int -> Sudoku -> Block
rows x s = (cells s) !! x

-- Get boxs = 3x3 square. There are 9 squares that form a sudoku
boxs :: (Int, Int) -> Sudoku -> Block
boxs (x,y) s =
      concat
      $ [take 3 (drop (x*3) rows) | rows <- take 3 (drop (y*3) (cells s))]
-----------------------------------
-- y determines rows, x determines columns. For instance, if y = 1, the function
-- will drop 1st 3 rows, with row 4-9 left. Take 3 takes 1st 3 members in list,
-- so row 7-9 are removed and only rows 4-6 left. If x = 1, rows 4-6 will drop 3
-- just they now contain only numbers from column 4-9. take 3 pick first 3 members
-- in list of each row, just only number from col 4-6 remain.
--(0,0) (1,0) (2,0)
--(0,1) (1,1) (2,1)
--(0,2) (1,2) (2,2) --> boxs by coordinates
-----------------------------------

-- | No block contains the same integer twice
-- >>> okSudoku allBlanks
-- True
-- >>> okSudoku $ fromString "36..712...5....18...92.47......13.284..5.2..927.46......53.89...83....6...769..43"
-- True
-- >>> okSudoku $ fromString "364871295752936184819254736596713428431582679278469351645328917983147562127695843"
-- True
-- Sudoku = [[Maybe Int]], [Block a]=[[a]], matrix a = [row a] =[[a]]

okSudoku :: Sudoku -> Bool
okSudoku s = and [okBlock b | b <- (things_to_check s)]

-- check if all the blocks are clean. List of alls blocks is things_to_check,
-- which compiles columns, rows and boxes
------------------------------------------------------
-- test. bads filter out blocks that do not meet the requirements
-- if bads is empty ==> null bads = True ==> prop_okSudoku = true or false = true

prop_okSudoku :: Sudoku -> Bool
prop_okSudoku s = okSudoku s || not (null bads)
  where bads = filter (not . okBlock) (things_to_check s)
-------------------------------------------------------

type Pos = (Int, Int)

-- | Return a blank position in the Sudoku
-- >>> blank allBlanks
-- (0,0)
-- >>> blank example
-- (0,2)
-- give the 1st element of list of all blank
blank :: Sudoku -> Pos
blank = head . blank'


-- determine position of a cell, rOw locates the row in sudoku then cOl locate the
-- position of cell on that row, i.e column
(!!!) :: [[a]] -> Pos -> a
(!!!) s (rOw,cOl) = (s !! rOw) !! cOl

-- Finds all blank spaces in a sudoku by finding cells where value is Nothing
blank' :: Sudoku -> [Pos]
blank' s = [ (rOw,cOl) | rOw <- [0..8], cOl <- [0..8], ((cells s) !!! (rOw,cOl)) == Nothing]

-- | Given a list, and a tuple containing an index in the list and a new value,
-- | update the given list with the new value at the given index.
-- >>> ["a","b","c","d"] !!= (1,"apa")
-- ["a","apa","c","d"]
-- >>> ["p","qq","rrr"] !!= (0,"bepa")
-- ["bepa","qq","rrr"]

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) list (num, v)
    | num < 0 || num > (length list)-1 = list
    | otherwise                 = (take num list) ++ [v] ++ (drop (num+1) list)

--in case number is invalid (no element exists in such position in the list), return
-- the list itself. if 0 <int < length of list -1, take first i member in list,
-- add the value x to that list then add the list with 1st i+1 element removed
-- since list indexing starts at 0, element at ith position is replaced.
-------------
-- test if the length of function unchanged after updating
prop_updatelist :: [a] -> (Int, a) -> Bool
prop_updatelist list (num,v) = length list == length (list !!= (num, v))
-----------
-- | Given a Sudoku, a position, and a new cell value,
-- | update the given Sudoku at the given position with the new value.


update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (y,x) val = Sudoku $ take y (cells s) ++ [row !!= (x, val)] ++ drop (y+1) (cells s)
                        where row = (cells s) !! y
-- work just like the update list (!!=) function
-- | solve takes an 81-character encoding of a Sudoku puzzle and returns a
-- | list of solutions for it, if any
-- String -> [String]
-- keep the same types as given so that main.hs can run without changing
-------------------
--thid section was created with the sole purpose of re-formating the type of
--input and output
solve :: String -> [String]
solve str = toString (convert_it (solve' sud))
    where sud =fromString str

convert_it :: Maybe Sudoku -> Sudoku
convert_it sud = fromJust sud
-------------------
solve' :: Sudoku -> Maybe Sudoku
solve' = solve_it blank (\s -> Nothing) --lambda expression/ anom function

solve_it :: (Sudoku -> Pos) -> (Sudoku -> Maybe Sudoku) -> Sudoku -> Maybe Sudoku
solve_it blank_cell pickanumber s
  | not (okSudoku s)      = Nothing -- case 1, sudoku not valid
  | noBlanks s            = Just s  -- case 2, sudoku is filled/solved
  | otherwise             = listToMaybe solutions -- listToMayBe transfrom [Sudoku] to Maybe Sudoku
    where
      solutions = [ fromJust sol | n <- [1..9],
                          let sol = solve_it blank_cell pickanumber (update s (blank_cell s) (Just n)),
                          sol /= Nothing]
-- pick a number from 1 and 9, trial and error
--------------------------------------------------
isAnswer :: Sudoku -> Sudoku -> Bool
isAnswer s1 s2 = isTheSameSudoku s1 s2 && noBlanks s1


--isTheSameSudoku checks that if 2 sudoku are exactly the same
-- any will return true if any 2 cells areDifferent. As such, 2 similar list of cells (or sudoku)
-- will return not False, which is True
isTheSameSudoku :: Sudoku -> Sudoku -> Bool
isTheSameSudoku s1 s2 = not $ any areDifferent $ zip (concat $ cells s1) (concat $ cells s2)
                     where areDifferent (x, y) = isJust x && isJust y && x /= y
-- zip make a lift of tuples, allow to compare s1 and s2 cells by cells
prop_solve' :: Sudoku -> Bool
prop_solve' s
    | solution == Nothing = True
    | otherwise           = isAnswer (fromJust solution) s
  where solution = solve' s


----------------------------------------------------
solve2 :: String -> [String]
solve2 str = toString (convert_it (solveX sud))
    where sud =fromString str

solveX :: Sudoku -> Maybe Sudoku
solveX = solve_it optimised_blanks (\s -> Nothing)
-- apply similar logic, same as backtracking method


-- This function simply calculate total number of blanks in row column and box
-- a blank locate for every blank, then pick the one with lowest
-- number of blanks in the row, column and box it locates. the program will
-- start solving with that box

optimised_blanks s  = snd. minimum $[(score p, p) | p <- blank' s]
      where
        score (y,x) = rowScore y + colScore x + boxScore (div x 3, div y 3)
        rowScore r  = number_of_blanks (rows r s)
        colScore c  = number_of_blanks (cols c s)
        boxScore b  = number_of_blanks (boxs b s)

-- filter out cells with nothing value, then count them. A block can be col, row
-- or box
number_of_blanks :: Block -> Int
number_of_blanks bl = length (filter (== Nothing) bl)

filter_Number :: Block -> [Maybe Int]
filter_Number bl = filter (/= Nothing) bl

