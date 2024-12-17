module Main (main) where

import Sudoku
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show $ sudoku $ puzzleSelect args

puzzleSelect :: Foldable t => t String -> Puzzle
puzzleSelect args
  | "k" `elem` args = killerPuzzle
  | "e" `elem` args = SudokuPuzzle emptyGrid
  | otherwise = puzzle