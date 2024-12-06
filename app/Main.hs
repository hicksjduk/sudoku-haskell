module Main (main) where

import Sudoku
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show $ sudoku (if "k" `elem` args then killerPuzzle else puzzle)
