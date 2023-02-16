module Sudoku where

import Data.List

type Grid = [[Int]]
type Coords = (Int, Int)

permittedValues = [1..9]
emptySquare = 0

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
      (s:_) -> Right s

validate :: Grid -> Either String Grid
validate grid
  | length grid /= gridSize = Left "Wrong number of rows"
  | any ((/= gridSize) . length) grid = Left "Wrong number of columns"
  | any (any (`notElem` emptySquare : permittedValues)) grid = Left "Invalid cell value"
  | any (hasDuplicates . (`rowValues` grid)) indices = Left "Row contains duplicate value(s)"
  | any (hasDuplicates . (`colValues` grid)) indices = Left "Column contains duplicate value(s)"
  | any (hasDuplicates . (`boxValues` grid)) boxTopCorners = Left "Box contains duplicate value(s)"
  | otherwise = Right grid
  where
    gridSize = length permittedValues
    hasDuplicates xs = length xs /= length (nub xs)
    indices = take gridSize [0..]
    (rowsPerBox, colsPerBox) = boxSize grid
    boxStarts perBox = takeWhile (< gridSize) $ iterate (+perBox) 0
    boxStartRows = boxStarts rowsPerBox
    boxStartCols = boxStarts colsPerBox
    boxTopCorners = [(r, c) | r <- boxStartRows, c <- boxStartCols]

solve :: Grid -> [Grid]
solve grid = case emptyCells grid of
  [] -> [grid]
  (square:_) -> solveAt square grid

emptyCells :: Grid -> [Coords]
emptyCells grid = concatMap coords colsByRow
  where
    coords (row, cols) = zip (repeat row) cols
    colsByRow = zip [0..] $ map (elemIndices emptySquare) grid

solveAt :: Coords -> Grid -> [Grid]
solveAt square grid = concatMap solveUsing $ allowedValues square grid
  where
    solveUsing value = solve $ setValueAt square value grid

setValueAt :: Coords -> a -> [[a]] -> [[a]]
setValueAt (row, col) value grid = replaceValueAt row newRow grid
  where newRow = replaceValueAt col value (grid !! row)

replaceValueAt :: Int -> a -> [a] -> [a]
replaceValueAt index value xs = case splitAt index xs of
  (_, []) -> xs
  ([], _:after) -> if index < 0 then xs else value : after
  (before, _:after) -> before ++ value : after

allowedValues :: Coords -> Grid -> [Int]
allowedValues square@(row, col) grid = permittedValues \\ blockedValues
  where
    blockedValues = concatMap ($ grid)
      [rowValues row, colValues col, boxValues square]

rowValues :: Int -> Grid -> [Int]
rowValues row grid = filter (/=emptySquare) $ grid !! row

colValues :: Int -> Grid -> [Int]
colValues col grid = filter (/=emptySquare) $ map (!! col) grid

boxValues :: Coords -> Grid -> [Int]
boxValues (row, col) grid = filter (/=emptySquare) values
  where
    (rowsPerBox, colsPerBox) = boxSize grid
    boxStart i perBox = (i `div` perBox) * perBox
    boxSection i perBox xs = take perBox $ drop (boxStart i perBox) xs
    boxRows = boxSection row rowsPerBox grid
    values = concatMap (boxSection col colsPerBox) boxRows

boxSize :: Grid -> (Int, Int)
boxSize grid = cache !! length grid
  where
    cache = map boxSize' [0..]
    boxSize' :: Int -> (Int, Int)
    boxSize' 0 = (0,0)
    boxSize' n = (rows, cols)
      where
        isDivisorOf :: Int -> Int -> Bool
        x `isDivisorOf` y = y `mod` x == 0
        squareRoot = sqrt $ fromIntegral n
        rows = n `div` cols
        cols = head $ filter (`isDivisorOf` n) [ceiling squareRoot..]
