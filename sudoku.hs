module Sudoku where

import Data.List

type Grid = [[Int]]

display :: Grid -> String
display grid = unlines $ map (unwords . map show) grid

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

sudoku :: Grid -> Grid
sudoku grid = head $ solve grid

solve :: Grid -> [Grid]
solve grid = case emptyCells grid of
  [] -> [grid]
  (square:_) -> solveAt square grid

emptyCells :: Grid -> [(Int, Int)]
emptyCells grid = concatMap coords colsByRow
  where
    coords (row, cols) = zip (repeat row) cols
    colsByRow = zip [0..] $ map (elemIndices 0) grid

solveAt :: (Int, Int) -> Grid -> [Grid]
solveAt square@(row, col) grid = concatMap solveUsing allowedValues
  where
    blockedValues = concatMap ($ grid) 
      [rowValues row, colValues col, boxValues square]
    allowedValues = [1..9] \\ blockedValues
    solveUsing value = solve $ setValueAt square value grid

setValueAt :: (Int, Int) -> a -> [[a]] -> [[a]]
setValueAt (row, col) value grid = replaceValueAt row newRow grid
  where newRow = replaceValueAt col value (grid !! row)

replaceValueAt :: Int -> a -> [a] -> [a]
replaceValueAt index value xs = case splitAt index xs of
  (_, []) -> xs
  ([], _:after) -> if index < 0 then xs else value : after
  (before, _:after) -> before ++ value : after

rowValues :: Int -> Grid -> [Int]
rowValues row grid = filter (/=0) $ grid !! row

colValues :: Int -> Grid -> [Int]
colValues col grid = filter (/=0) $ map (!! col) grid

boxValues :: (Int, Int) -> Grid -> [Int]
boxValues (row, col) grid = filter (/=0) values
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
