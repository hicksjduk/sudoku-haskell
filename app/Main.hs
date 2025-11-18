{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where
import Sudoku
import System.Environment
import Data.Foldable
import Data.Time.Clock

main = do
  args <- getArgs
  start <- getCurrentTime
  putStrLn $ printRes $ sudoku $ puzzleSelect $ toList args
  end <- getCurrentTime
  putStrLn $ unwords ["Done in", show $ diffUTCTime end start]

printRes :: Either String Grid -> String
printRes (Left s) = s
printRes (Right g) = unlines $ map (unwords . (map show)) g

puzzleSelect :: [String] -> Puzzle
puzzleSelect [] = puzzle
puzzleSelect ("k":_) = killer1
puzzleSelect ("k2":_) = killer2
puzzleSelect ("e":_) = sudokuPuzzle emptyGrid
