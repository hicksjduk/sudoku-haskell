module Debug where

import Sudoku

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

partialSolutions = let step sq = withValueAt sq (valueAt sq solution)
  in scanr step killerPuzzle $ emptySquares killerPuzzle
    