{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Sudokun where

import Data.List
import Data.Maybe
import Data.Function
import Combine
import Data.Ord
import Control.Parallel

permittedValues :: [Int]
permittedValues = [1..9]
emptySquare :: Int
emptySquare = 0
gridSize :: Int
gridSize = length permittedValues

sudoku :: Puzzle -> Either String Grid
sudoku p = if parametersValid then solveIt =<< validate p else Left "Invalid"
  where
    solveIt p = case solve p of
      [] -> Left "No solution found"
      (solution:_) -> Right solution

solve :: Puzzle -> [Grid]
solve p@(Puzzle _ grid) = case emptySquares p of
  [] -> [grid]
  (square:_) -> solveAt square p

solveAt :: Square -> Puzzle -> [Grid]
solveAt square p = solveParallel $ allowedValues square p
  where 
    solveParallel [] = []
    solveParallel vv@(v:vs) 
      | length vv > chunkSize = let solveOthers = solveParallel vs
          in par solveOthers $ solveUsing v ++ solveOthers
      | otherwise = concatMap solveUsing vv
    solveUsing value = solve $ withValueAt square value p
    chunkSize = 3

allowedValues :: Square -> Puzzle -> [Int]
allowedValues sq (Puzzle dims _) = 
  let dimsForSquare = filter (sq `inDimension`) dims
  in foldr1 intersect $ map possibleValues dimsForSquare

withValueAt :: Square -> Int -> Puzzle -> Puzzle
withValueAt sq i (Puzzle dims g) = 
  let newDims = mapMaybe (withoutValueAt sq i) dims
  in Puzzle newDims $ setValueAt sq i g

data Puzzle = Puzzle [Dimension] Grid deriving Show

type Grid = [[Int]]

type Square = (Int, Int)

type IntRange = (Int, Int)

type Box = (IntRange, IntRange)

data Dimension = Dimension [Square] [[Int]] DimensionType deriving (Eq, Show)

data DimensionType = RowD Int | ColumnD Int | BoxD Box | RegionD deriving (Eq, Show)

instance Ord Dimension where
  compare = 
    let criteria = [length . emptySquares, length . possibleValues] 
    in foldMap (compare `on`) criteria 

possibleValues :: Dimension -> [Int]
possibleValues (Dimension _ [vals] _) = vals
possibleValues (Dimension _ combs _) = nub $ concat combs

inDimension :: Square -> Dimension -> Bool
inDimension sq (Dimension _ _ (RowD row)) = sq `inRow` row
inDimension sq (Dimension _ _ (ColumnD col)) = sq `inColumn` col
inDimension sq (Dimension _ _ (BoxD box)) = sq `inBox` box
inDimension sq (Dimension emptySquares _ RegionD) = sq `elem` emptySquares

dimension :: [Square] -> Grid -> DimensionType -> Dimension
dimension squares grid = Dimension emptySquares [comb]
  where
    valuesBySquare = zip squares $ map (`valueAt` grid) squares
    (empty, notEmpty) = partition ((== emptySquare) . snd) valuesBySquare
    emptySquares = map fst empty
    comb = permittedValues \\ map snd notEmpty

rowDimension :: Int -> Grid -> Dimension
rowDimension row grid = 
  let sq = map (row,) $ take gridSize [0..]
  in dimension sq grid $ RowD row

inRow :: Square -> Int -> Bool
inRow (row, _) = (== row)

columnDimension :: Int -> Grid -> Dimension
columnDimension col grid = 
  let sq = map (, col) $ take gridSize [0..]
  in dimension sq grid $ ColumnD col

inColumn :: Square -> Int -> Bool
inColumn (_, col) = (== col)

boxDimension :: Box -> Grid -> Dimension
boxDimension box@((minRow, maxRow), (minCol, maxCol)) grid = 
  let sq = [(r, c) | r <- [minRow .. maxRow], c <- [minCol .. maxCol]]
  in dimension sq grid $ BoxD box

inBox :: Square -> Box -> Bool
(row, col) `inBox` (rows, cols) = inRange row rows && inRange col cols

inRange :: Int -> IntRange -> Bool
inRange v (minVal, maxVal) = v >= minVal && v <= maxVal

regionDimension :: Region -> Dimension
regionDimension (Region sq possibleCombs) = Dimension sq possibleCombs RegionD

data Region = Region {squares :: [Square], possibleCombinations :: [[Int]]} deriving (Show, Eq)

puzzleFrom :: [Dimension] -> Grid -> Puzzle
puzzleFrom dims = Puzzle (sort dims)

standardDimensions :: Grid -> [Dimension]
standardDimensions grid = dims
  where 
    dims = concat [rowDims, colDims, boxDims]
    rowDims = map (`rowDimension` grid) $ take gridSize [0..]
    colDims = map (`columnDimension` grid) $ take gridSize [0..]
    boxDims = map (`boxDimension` grid) $ boxes

sudokuPuzzle :: Grid -> Puzzle
sudokuPuzzle grid = puzzleFrom (standardDimensions grid) grid

killerPuzzle :: [Region] -> Puzzle
killerPuzzle regions = Puzzle dims emptyGrid
  where
    dims = killerDims ++ standardDimensions emptyGrid
    killerDims = map regionDimension regions

-- total :: Region -> Int
-- total (Region _ (c:_)) = sum c

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
puzzle = sudokuPuzzle
         [[8,0,0,0,0,0,0,0,0],
          [0,0,3,6,0,0,0,0,0],
          [0,7,0,0,9,0,2,0,0],
          [0,5,0,0,0,7,0,0,0],
          [0,0,0,0,4,5,7,0,0],
          [0,0,0,1,0,0,0,3,0],
          [0,0,1,0,0,0,0,6,8],
          [0,0,8,5,0,0,0,1,0],
          [0,9,0,0,0,0,4,0,0]]

emptyGrid :: Grid
emptyGrid = replicate gridSize $ replicate gridSize emptySquare

validate :: Puzzle -> Either String Puzzle
validate = Right
-- validate p@(SudokuPuzzle g) = validateGrid g >> return p
-- validate p@(KillerPuzzle rs g) = validateRegions rs g >> return p

-- validateGrid :: Grid -> Either String Grid
-- validateGrid grid
--   | length grid /= gridSize =
--     Left "Wrong number of rows"
--   | any ((/= gridSize) . length) grid =
--     Left "Wrong number of columns"
--   | any (any (`notElem` emptySquare : permittedValues)) grid =
--     Left "Invalid cell value"
--   | any (hasDuplicates . (`rowValues` grid)) indices =
--     Left "Row contains duplicate value(s)"
--   | any (hasDuplicates . (`colValues` grid)) indices =
--     Left "Column contains duplicate value(s)"
--   | any (hasDuplicates . (`boxValues` grid)) boxes =
--     Left "Box contains duplicate value(s)"
--   | otherwise = return grid
--   where
--     indices = take gridSize [0..]

-- validateRegions :: [Region] -> Grid -> Either String [Region]
-- validateRegions rs grid
--   | any totalOutOfRange rs =
--     Left "Region total out of permitted range"
--   | sum (map total rs) /= (gridSize * sum permittedValues) - (sum $ map sum grid) =
--     Left "Region totals incorrect"
--   | any sizeOutOfRange rs =
--     Left "Incorrect number of squares in regions"
--   | length sq /= length emptySq =
--     Left "Incorrect number of squares in regions"
--   | any (not . (`elem` emptySq)) sq =
--     Left "Square in region is not empty"
--   | any indexOutOfRange sq =
--     Left "Square index out of range"
--   | otherwise = return rs
--   where
--     sq = concatMap squares rs
--     emptySq = squaresContaining emptySquare grid
--     indexOutOfRange (row, col) = outOfRange row || outOfRange col
--     outOfRange n = n < 0 || n >= gridSize
--     sizeOutOfRange r = let len = length $ squares r
--       in len < 1 || len > gridSize

class SquareContainer c where
  emptySquares :: c -> [Square]

instance SquareContainer Puzzle where
  emptySquares (Puzzle dims _) = concatMap emptySquares dims

instance SquareContainer Dimension where
  emptySquares (Dimension sq _ _) = sq

squaresContaining :: Eq a => a -> [[a]] -> [Square]
squaresContaining v grid = 
  let squares row = map (row,)
  in concat $ zipWith squares [0..] $ map (elemIndices v) grid

replaceOrDeleteValueAt :: Int -> Maybe a -> [a] -> [a]
replaceOrDeleteValueAt i v xs = 
  let (before, _:after) = splitAt i xs
  in before ++ maybe after (:after) v

withoutValueAt :: Square -> Int -> Dimension -> Maybe Dimension
withoutValueAt _ _ (Dimension [_] _ _) = Nothing
withoutValueAt sq i (Dimension squares combs dimType) = Just $ Dimension newSq newCombs dimType
  where
    newSq = delete sq squares
    newCombs = mapMaybe (deleteIfPresent i) combs
  
deleteIfPresent :: (Eq a) => a -> [a] -> Maybe [a]
deleteIfPresent i xs = 
  let ys = delete i xs
  in if length xs == length ys then Nothing else Just ys

setValueAt :: Square -> Int -> Grid -> Grid
setValueAt (row, col) value grid =
  let newRow = replaceValueAt col value (grid !! row)
    in replaceValueAt row newRow grid

replaceValueAt :: Int -> a -> [a] -> [a]
replaceValueAt index value xs = let (before, _:after) = splitAt index xs
  in before ++ value : after

-- minRegionTotal :: Int -> Int
-- minRegionTotal count = cache !! count
--   where
--     cache = map mrt [0..]
--     mrt count = (sum . take count . sort) permittedValues

-- maxRegionTotal :: Int -> Int
-- maxRegionTotal count = cache !! count
--   where
--     cache = map mrt [0..]
--     mrt count = (sum . take count . sortBy (comparing Data.Ord.Down)) permittedValues

-- totalOutOfRange :: Region -> Bool
-- totalOutOfRange r = tot < minRegionTotal count || tot > maxRegionTotal count
--   where
--     count = length $ squares r
--     tot = total r

-- regionList :: [String] -> [(Char, Int)] -> [Region]
-- regionList pattern totals = sort $ map makeRegion (nub $ concat pattern)
--   where
--     makeRegion x = Region sq combs
--       where
--         sq = squaresContaining x pattern
--         total = fromJust $ lookup x totals
--         combs = combinations (length sq) total

-- combinations :: Int -> Int -> [[Int]]
-- combinations size total = cache !! size !! total
--   where
--     cache = map cacheBy [0..]
--     cacheBy s = map (combs s) [0..]
--     combs s t = filter ((== t) . sum) $ combineExactLength s permittedValues

-- regionSize :: Region -> Int
-- regionSize = length . squares

-- toPuzzle :: [String] -> [(Char, Int)] -> Puzzle
-- toPuzzle pattern totals = KillerPuzzle (regionList pattern totals) emptyGrid

-- -- Weekly 935
-- killerPuzzle :: Puzzle
-- killerPuzzle = toPuzzle
--   [
--     "aabbcddee",
--     "affcccgge",
--     "ffcchccgg",
--     "fijjhjjkg",
--     "iiljjjmkk",
--     "inlooompk",
--     "inooqoopk",
--     "nnorqsopp",
--     "nttrqsuup"
--   ]    [
--     ('a', 14),
--     ('b', 8),
--     ('c', 44),
--     ('d', 15),
--     ('e', 12),
--     ('f', 29),
--     ('g', 26),
--     ('h', 4),
--     ('i', 25),
--     ('j', 36),
--     ('k', 17),
--     ('l', 10),
--     ('m', 7),
--     ('n', 25),
--     ('o', 45),
--     ('p', 35),
--     ('q', 18),
--     ('r', 9),
--     ('s', 10),
--     ('t', 12),
--     ('u', 4)
--   ]

-- -- Daily 6617
-- killerPuzzle2 :: Puzzle
-- killerPuzzle2 = toPuzzle
--   [
--     "aabbcddef",
--     "gghcciief",
--     "jjhklmnno",
--     "pqqklmrro",
--     "psstttuuv",
--     "wxxyz122v",
--     "w33yz1455",
--     "6788994AA",
--     "67BB9CCDD"
--   ]    [
--     ('a', 14),
--     ('b', 8),
--     ('c', 16),
--     ('d', 10),
--     ('e', 9),
--     ('f', 13),
--     ('g', 7),
--     ('h', 11),
--     ('i', 9),
--     ('j', 10),
--     ('k', 9),
--     ('l', 10),
--     ('m', 12),
--     ('n', 11),
--     ('o', 7),
--     ('p', 8),
--     ('q', 10),
--     ('r', 11),
--     ('s', 12),
--     ('t', 11),
--     ('u', 16),
--     ('v', 4),
--     ('w', 10),
--     ('x', 9),
--     ('y', 17),
--     ('z', 6),
--     ('1', 7),
--     ('2', 12),
--     ('3', 9),
--     ('4', 11),
--     ('5', 8),
--     ('6', 16),
--     ('7', 10),
--     ('8', 5),
--     ('9', 19),
--     ('A', 11),
--     ('B', 11),
--     ('C', 4),
--     ('D', 12)
--   ]
