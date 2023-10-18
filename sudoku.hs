module Sudoku where

import Data.List

type Grid = [[Int]]
-- A square is represented by the indices of its row and column.
type Square = (Int, Int)
-- A box is represented by the ranges of its row and column indices.
type IntRange = (Int, Int)
type Box = (IntRange, IntRange)

permittedValues :: [Int]
permittedValues = [1..9]
emptySquare :: Int 
emptySquare = 0

gridSize :: Int
gridSize = length permittedValues

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

puzzle :: Grid
puzzle = [[8,0,0,0,0,0,0,0,0],
          [0,0,3,6,0,0,0,0,0],
          [0,7,0,0,9,0,2,0,0],
          [0,5,0,0,0,7,0,0,0],
          [0,0,0,0,4,5,7,0,0],
          [0,0,0,1,0,0,0,3,0],
          [0,0,1,0,0,0,0,6,8],
          [0,0,8,5,0,0,0,1,0],
          [0,9,0,0,0,0,4,0,0]]

sudoku :: Grid -> Either String Grid
sudoku grid = if parametersValid then solveIt =<< validate grid else Left "Invalid"
  where
    solveIt grid = case solve grid of
      [] -> Left "No solution found"
      (solution:_) -> Right solution

validate :: Grid -> Either String Grid
validate grid
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
  | otherwise = Right grid
  where
    indices = take gridSize [0..]

solve :: Grid -> [Grid]
solve grid = case emptyCells grid of
  [] -> [grid]
  (square:_) -> solveAt square grid

emptyCells :: Grid -> [Square]
emptyCells grid = concat $ zipWith squares [0..] $ map (elemIndices emptySquare) grid
  where
    squares row = map (row,)

solveAt :: Square -> Grid -> [Grid]
solveAt square grid = concatMap solveUsing $ allowedValues square grid
  where
    solveUsing value = solve $ setValueAt square value grid

setValueAt :: Square -> Int -> Grid -> Grid
setValueAt (row, col) value grid = replaceValueAt row newRow grid
  where 
    newRow = replaceValueAt col value (grid !! row)

replaceValueAt :: Int -> a -> [a] -> [a]
replaceValueAt index value xs = before ++ value : after
  where 
    (before, _:after) = splitAt index xs

allowedValues :: Square -> Grid -> [Int]
allowedValues square@(row, col) grid = permittedValues \\ blockedValues
  where
    blockedValues = concatMap ($ grid)
      [rowValues row, colValues col, boxValues $ boxContaining square]

rowValues :: Int -> Grid -> [Int]
rowValues row grid = filter (/= emptySquare) $ grid !! row

colValues :: Int -> Grid -> [Int]
colValues col grid = filter (/= emptySquare) $ map (!! col) grid

isInBox :: Square -> Box -> Bool
(row, col) `isInBox` (rows, cols) =
  inRange row rows && inRange col cols
  where
    inRange n (min, max) = n >= min && n <= max

boxContaining :: Square -> Box
boxContaining square = head $ filter (square `isInBox`) boxes

boxValues :: Box -> Grid -> [Int]
boxValues (rows, cols) grid = 
  filter (/= emptySquare) values
  where
    slice (first, last) = take (last - first + 1) . drop first
    values = concatMap (slice cols) $ slice rows grid
    