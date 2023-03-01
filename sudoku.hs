module Sudoku where

import Data.List

type Grid = [[Int]]
-- A square is represented by the indices of its row and column.
type Square = (Int, Int)
-- A box is represented by its top left and bottom right squares.
type Box = (Square, Square)

{-
None of the code below makes any assumption about the size of the grid or
the permitted values in it. Everything is driven by the two assignments
that follow this comment. So (for example) to work with puzzles that have
six rows and columns, you could assign a list of six numbers to
permittedValues. The only (sensible) constraints on this (which make
the solver fail with an error if violated) are that:
 - permittedValues should not be empty.
 - the length of permittedValues should not be a prime number.
 - emptySquare should not be set to a value that is in permittedValues.
-}
permittedValues :: [Int]
permittedValues = [1..9]
emptySquare :: Int 
emptySquare = 0

gridSize :: Int
gridSize = length permittedValues

{-
Calculation of the box size assumes that the boxes follow the convention
that:
 - Each box is as nearly square as possible - that is, the number of rows
   and the number of columns in each box differ by the smallest possible
   amount.
 - If the box is not square (because the grid size is not a square number),
   it has more columns than rows.
This implies that the number of columns in a box is the smallest divisor
of the grid size that is not less than its square root, and the number of 
rows in a box is the grid size divided by the number of columns.
-}
boxSize :: (Int, Int)
boxSize = if gridSize == 0 then (0, 0) else (rows, cols)
  where
    x `isDivisorOf` y = y `mod` x == 0
    squareRoot = sqrt $ fromIntegral gridSize
    rows = gridSize `div` cols
    cols = head $ filter (`isDivisorOf` gridSize) [ceiling squareRoot..]

boxes :: [Box]
boxes = map boxExtent boxTopCorners
  where
    (rowsPerBox, colsPerBox) = boxSize
    boxStarts perBox = takeWhile (< gridSize) $ iterate (+ perBox) 0
    boxStartRows = boxStarts rowsPerBox
    boxStartCols = boxStarts colsPerBox
    boxTopCorners = [(r, c) | r <- boxStartRows, c <- boxStartCols]
    boxExtent topLeft@(topRow, leftCol) = (topLeft, bottomRight)
      where
        bottomRight = (topRow + rowsPerBox - 1, leftCol + colsPerBox - 1)

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
sudoku grid = solveIt =<< validate grid
  where
    solveIt grid = case solve grid of
      [] -> Left "No solution found"
      (solution:_) -> Right solution

validate :: Grid -> Either String Grid
validate grid
  | gridSize == 0 =
    Left "Grid size is 0"
  | boxSize == (1, gridSize) =
    Left "Grid size is a prime number"
  | emptySquare `elem` permittedValues =
    Left "Empty square value is also a permitted value"
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
    hasDuplicates xs = length xs /= length (nub xs)
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
      [rowValues row, colValues col, boxValues (boxContaining square)]

rowValues :: Int -> Grid -> [Int]
rowValues row grid = filter (/= emptySquare) $ grid !! row

colValues :: Int -> Grid -> [Int]
colValues col grid = filter (/= emptySquare) $ map (!! col) grid

isInBox :: Square -> Box -> Bool
(row, col) `isInBox` ((topRow, leftCol), (bottomRow, rightCol)) =
  inRange topRow bottomRow row && inRange leftCol rightCol col
  where
    inRange min max n = n >= min && n <= max

boxContaining :: Square -> Box
boxContaining square = head $ filter (square `isInBox`) boxes

boxValues :: Box -> Grid -> [Int]
boxValues ((topRow, leftCol), (bottomRow, rightCol)) grid = 
  filter (/= emptySquare) values
  where
    boxSection top bottom xs = take (bottom - top + 1) $ drop top xs
    boxRows = boxSection topRow bottomRow grid
    values = concatMap (boxSection leftCol rightCol) boxRows