module Main (main) where

import Sudoku
import System.Environment
import Data.Foldable

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show $ solve $ puzzleSelect $ tail $ toList args

puzzleSelect :: [String] -> Puzzle
puzzleSelect [] = puzzle
puzzleSelect ("k":_) = killerPuzzle
puzzleSelect ("e":_) = SudokuPuzzle emptyGrid
puzzleSelect (a:_) = let i = read a :: Int
  in partialSolutions !! i

solution :: Grid
solution =
    [[4,8,1,7,2,9,6,3,5],
     [2,6,7,5,8,3,1,9,4],
     [3,5,9,4,1,6,7,2,8],
     [8,1,5,2,3,4,9,7,6],
     [9,2,6,8,7,1,5,4,3],
     [7,3,4,9,6,5,2,8,1],
     [6,9,3,1,4,7,8,5,2],
     [1,7,2,3,5,8,4,6,9],
     [5,4,8,6,9,2,3,1,7]]

partialSolutions :: [Puzzle]
partialSolutions = let nextOne sq = withValueAt sq (valueAt sq solution)
  in scanr nextOne killerPuzzle $ reverse $ emptySquares killerPuzzle
    