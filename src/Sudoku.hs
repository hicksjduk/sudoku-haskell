{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid reverse" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Sudoku where

import Data.List
import Data.Maybe
import Data.Function
import Combine
import Control.Parallel

permittedValues :: [Int]
permittedValues = [1..9]
emptySquare :: Int
emptySquare = 0

gridSize :: Int
gridSize = length permittedValues

type Grid = [[Int]]
-- A square is represented by the indices of its row and column.
type Square = (Int, Int)
-- A box is represented by the ranges of its row and column indices.
type IntRange = (Int, Int)
type Box = (IntRange, IntRange)

data Puzzle = SudokuPuzzle Grid | KillerPuzzle [Region] Grid deriving Show

-- A region has a list of squares, and a list of possible combinations of values,
-- all of which add up to a specific total.
data Region = Region {squares :: [Square], possibleCombinations :: [[Int]]} deriving (Show, Eq)

-- The total which each combination adds up to: if the list of combinations is
-- empty it is 0, otherwise it is the sum of the first combination.
total :: Region -> Int
total (Region _ []) = 0
total (Region _ (c:_)) = sum c

empty :: Region -> Bool
empty (Region [] _) = True
empty _ = False

withoutValue :: Region -> Int -> Region
(Region [_] _) `withoutValue` _ = Region [] []
(Region (_:squares) combs) `withoutValue` value =
  let newCombs = mapMaybe (deleteIfPresent value) combs
  in Region squares newCombs

deleteIfPresent :: Eq a => a -> [a] -> Maybe [a]
deleteIfPresent value xs = deleteAt =<< elemIndex value xs
  where
    deleteAt i = let (before, _:after) = splitAt i xs
      in Just $ before ++ after

parametersValid :: Bool
parametersValid
  | gridSize == 0 =
    error "Grid size is 0"
  | hasDuplicates permittedValues =
    error "Permitted values include duplicate(s)"
  | emptySquare `elem` permittedValues =
    error "Empty square value is also a permitted value"
  | otherwise = True

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

boxSize :: (Int, Int)
boxSize = (rows, cols)
  where
    x `isDivisorOf` y = y `mod` x == 0
    squareRoot = sqrt $ fromIntegral gridSize
    rows = gridSize `div` cols
    cols = head $ filter (`isDivisorOf` gridSize) [ceiling squareRoot..]

boxes :: [Box]
boxes = (,) <$> rowRanges <*> colRanges
  where
    (rowsPerBox, colsPerBox) = boxSize
    ranges perBox = map (rangeFrom perBox) [0, perBox .. gridSize - 1]
    rangeFrom size x = (x, x + size - 1)
    rowRanges = ranges rowsPerBox
    colRanges = ranges colsPerBox

puzzle :: Puzzle
puzzle = SudokuPuzzle
         [[8,0,0,0,0,0,0,0,0],
          [0,0,3,6,0,0,0,0,0],
          [0,7,0,0,9,0,2,0,0],
          [0,5,0,0,0,7,0,0,0],
          [0,0,0,0,4,5,7,0,0],
          [0,0,0,1,0,0,0,3,0],
          [0,0,1,0,0,0,0,6,8],
          [0,0,8,5,0,0,0,1,0],
          [0,9,0,0,0,0,4,0,0]]

sudoku :: Puzzle -> Either String Grid
sudoku p = if parametersValid then solveIt =<< validate p else Left "Invalid"
  where
    solveIt p = case solve p of
      [] -> Left "No solution found"
      (solution:_) -> Right solution

emptyGrid :: Grid
emptyGrid = replicate gridSize $ replicate gridSize emptySquare

validate :: Puzzle -> Either String Puzzle
validate p@(SudokuPuzzle g) = validateGrid g >> return p
validate p@(KillerPuzzle rs _) = validateRegions rs >> return p

validateGrid :: Grid -> Either String Grid
validateGrid grid
  | length grid /= gridSize =
    Left "Wrong number of rows"
  | any ((/= gridSize) . length) grid =
    Left "Wrong number of columns"
  | any (any (`notElem` emptySquare : permittedValues)) grid =
    Left "Invalid cell value"
  | any (hasDuplicates . (`rowValues` grid)) indices =
    Left "Row contains duplicate value(s)"
  | any (hasDuplicates . (`colValues` grid)) indices =
    Left "Column contains duplicate value(s)"
  | any (hasDuplicates . (`boxValues` grid)) boxes =
    Left "Box contains duplicate value(s)"
  | otherwise = return grid
  where
    indices = take gridSize [0..]

validateRegions :: [Region] -> Either String [Region]
validateRegions rs
  | any totalOutOfRange rs =
    Left "Region total out of permitted range"
  | sum (map total rs) /= (gridSize * sum permittedValues) =
    Left "Region totals incorrect"
  | length sq /= (gridSize * gridSize) =
    Left "Incorrect number of squares in regions"
  | any indexOutOfRange sq =
    Left "Square index out of range"
  | otherwise = return rs
  where
    sq = concatMap squares rs
    indexOutOfRange (row, col) = outOfRange row || outOfRange col
    outOfRange n = n < 0 || n >= gridSize

solve :: Puzzle -> [Grid]
solve p = case emptySquares p of
  [] -> [grid p]
  (square:_) -> solveAt square p

grid :: Puzzle -> Grid
grid (SudokuPuzzle g) = g
grid (KillerPuzzle _ g) = g

valueAt :: Square -> Grid -> Int
valueAt (row, col) grid = grid !! row !! col

emptySquares :: Puzzle -> [Square]
emptySquares (SudokuPuzzle grid) = squaresContaining emptySquare grid
emptySquares (KillerPuzzle rs _) = concatMap squares rs

squaresContaining :: Eq a => a -> [[a]] -> [Square]
squaresContaining v grid = let squares row = map (row,)
  in concat $ zipWith squares [0..] $ map (elemIndices v) grid

solveAt :: Square -> Puzzle -> [Grid]
solveAt square p = concatMap solveUsing $ allowedValues square p
  where
    solveUsing value = solve $ withValueAt square value p

withValueAt :: Square -> Int -> Puzzle -> Puzzle
withValueAt sq i (SudokuPuzzle g) = SudokuPuzzle $ setValueAt sq i g
withValueAt sq i (KillerPuzzle rs g) = KillerPuzzle regions $ setValueAt sq i g
  where 
    (r:rr) = rs
    nr = r `withoutValue` i
    regions = if empty nr then rr else nr : rr

setValueAt :: Square -> Int -> Grid -> Grid
setValueAt (row, col) value grid =
  let newRow = replaceValueAt col value (grid !! row)
    in replaceValueAt row newRow grid

replaceValueAt :: Int -> a -> [a] -> [a]
replaceValueAt index value xs = let (before, _:after) = splitAt index xs
  in before ++ value : after

allowedValues :: Square -> Puzzle -> [Int]
allowedValues square@(row, col) p = foldl1 (\\) $ possible : blocked
  where
    possible = possibleValuesAt square p
    blocked = map ($ grid p)
      [rowValues row, colValues col, boxValues $ boxContaining square]

possibleValuesAt :: Square -> Puzzle -> [Int]
possibleValuesAt _ (SudokuPuzzle _) = permittedValues
possibleValuesAt _ (KillerPuzzle (r:_) _) = nub $ concat $ possibleCombinations r

rowValues :: Int -> Grid -> [Int]
rowValues row grid = filter (/= emptySquare) $ grid !! row

colValues :: Int -> Grid -> [Int]
colValues col grid = filter (/= emptySquare) $ map (!! col) grid

boxContaining :: Square -> Box
boxContaining square = head $ filter (square `isInBox`) boxes
  where
    (row, col) `isInBox` (rows, cols) = inRange row rows && inRange col cols
    inRange n (min, max) = n >= min && n <= max

boxValues :: Box -> Grid -> [Int]
boxValues (rows, cols) grid =
  filter (/= emptySquare) values
  where
    slice (first, lastInc) = drop first . take (lastInc + 1)
    values = concatMap (slice cols) $ slice rows grid

minRegionTotal :: Int -> Int
minRegionTotal count = cache !! count
  where
    cache = map mrt [0..]
    mrt count = (sum . take count . sort) permittedValues

maxRegionTotal :: Int -> Int
maxRegionTotal count = cache !! count
  where
    cache = map mrt [0..]
    mrt count = (sum . take count . reverse . sort) permittedValues

totalOutOfRange :: Region -> Bool
totalOutOfRange r = tot < minRegionTotal count || tot > maxRegionTotal count
  where
    count = length $ squares r
    tot = total r

regionContaining :: Square -> [Region] -> Region
regionContaining (row, col) rs = cache !! row !! col
  where
    cache = map forRow [0..]
    forRow r = map (rc . (r,)) [0..]
    rc sq = head $ filter ((sq `elem`) . squares) rs

possibleRegionValues :: Region -> Grid -> [Int]
possibleRegionValues region grid = nub $ concat combs \\ existing
  where
    combs = filter (existing `isSubsetOf`) $ possibleCombinations region
    xs `isSubsetOf` ys = all (`elem` ys) xs
    existing = filter (/= emptySquare) $ map (`valueAt` grid) $ squares region

regions :: [String] -> [(Char, Int)] -> [Region]
regions pattern totals = sortBy sortThem $ map makeRegion (nub $ concat pattern)
  where
    sortThem = foldMap (compare `on`) criteria
    criteria = [length . possibleCombinations, length . squares]
    makeRegion x = Region sq combs
      where
        sq = squaresContaining x pattern
        total = fromJust $ lookup x totals
        combs = combinations (length sq) total

combinations :: Int -> Int -> [[Int]]
combinations size total = cache !! size !! total
  where
    cache = map cacheBy [0..]
    cacheBy s = map (combs s) [0..]
    combs s t = filter ((== t) . sum) $ combineExactLength s permittedValues

regionSize :: Region -> Int
regionSize = length . squares

toPuzzle :: [String] -> [(Char, Int)] -> Puzzle
toPuzzle pattern totals = KillerPuzzle (regions pattern totals) emptyGrid

-- Weekly 935
killerPuzzle :: Puzzle
killerPuzzle = toPuzzle
  [
    "aabbcddee",
    "affcccgge",
    "ffcchccgg",
    "fijjhjjkg",
    "iiljjjmkk",
    "inlooompk",
    "inooqoopk",
    "nnorqsopp",
    "nttrqsuup"
  ]    [
    ('a', 14),
    ('b', 8),
    ('c', 44),
    ('d', 15),
    ('e', 12),
    ('f', 29),
    ('g', 26),
    ('h', 4),
    ('i', 25),
    ('j', 36),
    ('k', 17),
    ('l', 10),
    ('m', 7),
    ('n', 25),
    ('o', 45),
    ('p', 35),
    ('q', 18),
    ('r', 9),
    ('s', 10),
    ('t', 12),
    ('u', 4)
  ]

-- Daily 6617
killerPuzzle2 :: Puzzle
killerPuzzle2 = toPuzzle
  [
    "aabbcddef",
    "gghcciief",
    "jjhklmnno",
    "pqqklmrro",
    "psstttuuv",
    "wxxyz122v",
    "w33yz1455",
    "6788994AA",
    "67BB9CCDD"
  ]    [
    ('a', 14),
    ('b', 8),
    ('c', 16),
    ('d', 10),
    ('e', 9),
    ('f', 13),
    ('g', 7),
    ('h', 11),
    ('i', 9),
    ('j', 10),
    ('k', 9),
    ('l', 10),
    ('m', 12),
    ('n', 11),
    ('o', 7),
    ('p', 8),
    ('q', 10),
    ('r', 11),
    ('s', 12),
    ('t', 11),
    ('u', 16),
    ('v', 4),
    ('w', 10),
    ('x', 9),
    ('y', 17),
    ('z', 6),
    ('1', 7),
    ('2', 12),
    ('3', 9),
    ('4', 11),
    ('5', 8),
    ('6', 16),
    ('7', 10),
    ('8', 5),
    ('9', 19),
    ('A', 11),
    ('B', 11),
    ('C', 4),
    ('D', 12)
  ]
