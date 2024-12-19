module Main (main) where

import Sudoku
import System.Environment
import Data.Foldable
import Data.Time.Clock

main :: IO ()
main = do
  args <- getArgs
  start <- getCurrentTime
  putStrLn $ printRes $ sudoku $ puzzleSelect $ tail $ toList args
  end <- getCurrentTime
  putStrLn $ unwords ["Done in", show $ diffUTCTime end start]

printRes :: Either String Grid -> String
printRes (Left s) = s
printRes (Right g) = show g

puzzleSelect :: [String] -> Puzzle
puzzleSelect [] = puzzle
puzzleSelect ("k":_) = killerPuzzle
puzzleSelect ("k2":_) = killerPuzzle2
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
    