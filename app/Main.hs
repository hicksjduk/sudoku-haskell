{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module Main (main) where

import Sudoku
import System.Environment
import Data.Foldable
import Data.Time.Clock
import Data.List

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
puzzleSelect ("k":_) = killer1
puzzleSelect ("k2":_) = killer2
puzzleSelect ("e":_) = sudokuPuzzle emptyGrid
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
partialSolutions = reverse $ unfoldr nextOne killer1
  where
    nextOne p = case emptySquareData p of
      [] -> Nothing
      (sd@(SquareData sq _):_) -> 
        return (p, withValueAt sd (valueAt sq solution) p)
    