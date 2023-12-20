module Sudoku where

import Data.List
import Data.Maybe

permittedValues :: [Int]
permittedValues = [1..9]
emptySquare :: Int 
emptySquare = 0

gridSize :: Int
gridSize = length permittedValues

type Grid = [[Int]]
-- A square is represented by the indices of its row and column.
type Square = (Int, Int)
-- A box is represented by the ranges of its row and column indices.
type IntRange = (Int, Int)
type Box = (IntRange, IntRange)

data Region = Region {squares :: [Square], total :: Int} deriving Show

data Puzzle = SudokuPuzzle Grid | KillerPuzzle [Region] Grid

parametersValid :: Bool
parametersValid 
  | gridSize == 0 =
    error "Grid size is 0"
  | hasDuplicates permittedValues =
    error "Permitted values include duplicate(s)"
  | emptySquare `elem` permittedValues =
    error "Empty square value is also a permitted value"
  | otherwise = True

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

boxSize :: (Int, Int)
boxSize = (rows, cols)
  where
    x `isDivisorOf` y = y `mod` x == 0
    squareRoot = sqrt $ fromIntegral gridSize
    rows = gridSize `div` cols
    cols = head $ filter (`isDivisorOf` gridSize) [ceiling squareRoot..]

boxes :: [Box]
boxes = (,) <$> rowRanges <*> colRanges
  where
    (rowsPerBox, colsPerBox) = boxSize
    ranges perBox = map (rangeFrom perBox) [0, perBox .. gridSize - 1]
    rangeFrom size x = (x, x + size - 1)
    rowRanges = ranges rowsPerBox
    colRanges = ranges colsPerBox

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
sudoku grid = if parametersValid then solveIt =<< validate grid else Left "Invalid"
  where
    solveIt grid = case solve $ SudokuPuzzle grid of
      [] -> Left "No solution found"
      (solution:_) -> Right solution

killer :: KillerStructure -> Either String Grid
killer k = case solve $ KillerPuzzle (regions k) emptyGrid of
  [] -> Left "No solution found"
  (solution:_) -> Right solution
  where emptyGrid = replicate gridSize (replicate gridSize emptySquare)

validate :: Grid -> Either String Grid
validate grid
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
    indices = take gridSize [0..]

solve :: Puzzle -> [Grid]
solve p = case emptyCells g of
  [] -> [g]
  (square:_) -> solveAt square p
  where
    g = grid p

emptyCells :: Grid -> [Square]
emptyCells = squaresContaining emptySquare
  
squaresContaining :: Eq a => a -> [[a]] -> [Square]
squaresContaining v grid = let squares row = map (row,)
  in concat $ zipWith squares [0..] $ map (elemIndices v) grid

solveAt :: Square -> Puzzle -> [Grid]
solveAt square p = let solveUsing value = solve $ withValueAt square value p
  in concatMap solveUsing $ allowedValues square p   

setValueAt :: Square -> Int -> Grid -> Grid
setValueAt (row, col) value grid = let newRow = replaceValueAt col value (grid !! row)
  in replaceValueAt row newRow grid

replaceValueAt :: Int -> a -> [a] -> [a]
replaceValueAt index value xs = let (before, _:after) = splitAt index xs
  in before ++ value : after

allowedValues :: Square -> Puzzle -> [Int]
allowedValues square@(row, col) p = foldl1 (\\) $ possible : blocked
  where
    possible = possibleValuesAt square p
    blocked = map ($ grid p)
      [rowValues row, colValues col, boxValues $ boxContaining square]

rowValues :: Int -> Grid -> [Int]
rowValues row grid = filter (/= emptySquare) $ grid !! row

colValues :: Int -> Grid -> [Int]
colValues col grid = filter (/= emptySquare) $ map (!! col) grid

isInBox :: Square -> Box -> Bool
(row, col) `isInBox` (rows, cols) =
  inRange row rows && inRange col cols
  where
    inRange n (min, max) = n >= min && n <= max

boxContaining :: Square -> Box
boxContaining square = head $ filter (square `isInBox`) boxes

boxValues :: Box -> Grid -> [Int]
boxValues (rows, cols) grid = 
  filter (/= emptySquare) values
  where
    slice (first, lastInc) = drop first . take (lastInc + 1)
    values = concatMap (slice cols) $ slice rows grid

grid :: Puzzle -> Grid
grid (SudokuPuzzle g) = g
grid (KillerPuzzle _ g) = g

withValueAt :: Square -> Int -> Puzzle -> Puzzle
withValueAt sq i (SudokuPuzzle g) = SudokuPuzzle $ setValueAt sq i g
withValueAt sq i (KillerPuzzle r g) = KillerPuzzle r $ setValueAt sq i g

combinations :: (Eq a, Num a) => Int -> a -> [a] -> [[a]]
combinations 1 targetSum xs = [[targetSum] | targetSum `elem` xs]
combinations targetLength targetSum xs = concatMap combinationsAt [0 .. length xs - targetLength]
  where
    combinationsAt n = let (y:ys) = drop n xs in
      map (y:) (combinations (targetLength-1) (targetSum-y) ys)

regionValues :: Region -> Grid -> [Int]
regionValues region grid = filter (/= emptySquare) values
    where
        values = map valueAt $ squares region
        valueAt (row, col) = grid!!row!!col

possibleRegionValues :: Region -> Grid -> [Int]
possibleRegionValues region grid = nub $ concat $ combinations targetLength targetSum possibleValues
    where
        knownValues = regionValues region grid
        targetLength = length (squares region) - length knownValues
        targetSum = total region - sum knownValues
        possibleValues = permittedValues \\ knownValues

possibleValuesAt :: Square -> Puzzle -> [Int]
possibleValuesAt _ (SudokuPuzzle _) = permittedValues
possibleValuesAt sq (KillerPuzzle r g) = possibleRegionValues (regionContaining sq r) g

regionContaining :: Square -> [Region] -> Region
regionContaining sq regions = head $ filter ((sq `elem`) . squares) regions

data KillerStructure = KillerStructure {pattern :: [String], totals :: [(Char, Int)]}

regions :: KillerStructure -> [Region]
regions k = map makeRegion (nub $ concat $ pattern k)
    where
        makeRegion x = Region squares total
            where
                squares = squaresContaining x $ pattern k
                total = fromJust $ lookup x $ totals k

killerPuzzle = KillerStructure
    [
        "aabbcddee",
        "affcccgge",
        "ffcchccgg",
        "fijjhjjkg",
        "iiljjjmkk",
        "inlooompk",
        "inooqoopk",
        "nnorqsopp",
        "nttrqsuup"
    ]    [
        ('a', 14),
        ('b', 8),
        ('c', 44),
        ('d', 15),
        ('e', 12),
        ('f', 29),
        ('g', 26),
        ('h', 4),
        ('i', 25),
        ('j', 36),
        ('k', 17),
        ('l', 10),
        ('m', 7),
        ('n', 25),
        ('o', 45),
        ('p', 35),
        ('q', 18),
        ('r', 9),
        ('s', 10),
        ('t', 12),
        ('u', 4)
    ]
