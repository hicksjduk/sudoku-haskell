module Sudoku where

import Data.List
import Data.Maybe
import Data.Either

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

data Puzzle = SudokuPuzzle Grid | KillerPuzzle [Region] Grid deriving Show

data Region = Region {squares :: [Square], total :: Int} deriving (Show, Eq)

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

puzzle :: Puzzle
puzzle = SudokuPuzzle 
         [[8,0,0,0,0,0,0,0,0],
          [0,0,3,6,0,0,0,0,0],
          [0,7,0,0,9,0,2,0,0],
          [0,5,0,0,0,7,0,0,0],
          [0,0,0,0,4,5,7,0,0],
          [0,0,0,1,0,0,0,3,0],
          [0,0,1,0,0,0,0,6,8],
          [0,0,8,5,0,0,0,1,0],
          [0,9,0,0,0,0,4,0,0]]

sudoku :: Puzzle -> Either String Grid
sudoku p = if parametersValid then solveIt =<< validate p else Left "Invalid"
  where
    solveIt p = case solve p of
      [] -> Left "No solution found"
      (solution:_) -> Right solution

emptyGrid :: Grid
emptyGrid = replicate gridSize $ replicate gridSize emptySquare

validate :: Puzzle -> Either String Puzzle
validate p@(SudokuPuzzle g) = case validateGrid g of
  (Left s) -> Left s
  (Right _) -> Right p
validate p@(KillerPuzzle rs _) = case validateRegions rs of
  (Left s) -> Left s
  (Right _) -> Right p

validateGrid :: Grid -> Either String Grid
validateGrid grid
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

validateRegions :: [Region] -> Either String [Region]
validateRegions rs
  | any totalOutOfRange rs = 
    Left "Region total out of permitted range"
  | sum (map total rs) /= (gridSize * sum permittedValues) = 
    Left "Region totals incorrect"
  | length sq /= (gridSize * gridSize) =
    Left "Incorrect number of squares in regions"
  | any indexOutOfRange sq = 
    Left "Square index out of range"
  | otherwise = Right rs
  where
    sq = concatMap squares rs
    indexOutOfRange (row, col) = outOfRange row || outOfRange col
    outOfRange n = n < 0 || n >= gridSize

minRegionValue :: Int -> Int
minRegionValue count = (sum . take count . sort) permittedValues

maxRegionValue :: Int -> Int
maxRegionValue count = (sum . take count . reverse . sort) permittedValues

totalOutOfRange :: Region -> Bool
totalOutOfRange r = tot < minRegionValue count || tot > maxRegionValue count
  where
    count = length $ squares r
    tot = total r

solve :: Puzzle -> [Grid]
solve p = case emptySquares p of
  [] -> [grid p]
  (square:_) -> solveAt square p

grid :: Puzzle -> Grid
grid (SudokuPuzzle g) = g
grid (KillerPuzzle _ g) = g

valueAt :: Square -> Grid -> Int
valueAt (row, col) grid = grid !! row !! col

emptySquares :: Puzzle -> [Square]
emptySquares (SudokuPuzzle grid) = squaresContaining emptySquare grid
emptySquares (KillerPuzzle rs grid) = concatMap empties rs
  where
    empties = filter ((== emptySquare) . (`valueAt` grid)) . squares

squaresContaining :: Eq a => a -> [[a]] -> [Square]
squaresContaining v grid = let squares row = map (row,)
  in concat $ zipWith squares [0..] $ map (elemIndices v) grid

solveAt :: Square -> Puzzle -> [Grid]
solveAt square p = let solveUsing value = solve $ withValueAt square value p
  in concatMap solveUsing $ allowedValues square p

withValueAt :: Square -> Int -> Puzzle -> Puzzle
withValueAt sq i (SudokuPuzzle g) = SudokuPuzzle $ setValueAt sq i g
withValueAt sq i (KillerPuzzle rs g) = KillerPuzzle rs $ setValueAt sq i g

setValueAt :: Square -> Int -> Grid -> Grid
setValueAt (row, col) value grid =
  let newRow = replaceValueAt col value (grid !! row)
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

possibleValuesAt :: Square -> Puzzle -> [Int]
possibleValuesAt _ (SudokuPuzzle _) = permittedValues
possibleValuesAt sq (KillerPuzzle rs g) =
  possibleRegionValues (regionContaining sq rs) g

rowValues :: Int -> Grid -> [Int]
rowValues row grid = filter (/= emptySquare) $ grid !! row

colValues :: Int -> Grid -> [Int]
colValues col grid = filter (/= emptySquare) $ map (!! col) grid

boxContaining :: Square -> Box
boxContaining square = head $ filter (square `isInBox`) boxes
  where
    (row, col) `isInBox` (rows, cols) = inRange row rows && inRange col cols
    inRange n (min, max) = n >= min && n <= max

boxValues :: Box -> Grid -> [Int]
boxValues (rows, cols) grid =
  filter (/= emptySquare) values
  where
    slice (first, lastInc) = drop first . take (lastInc + 1)
    values = concatMap (slice cols) $ slice rows grid

regionContaining :: Square -> [Region] -> Region
regionContaining sq rs = head $ filter ((sq `elem`) . squares) rs

possibleRegionValues :: Region -> Grid -> [Int]
possibleRegionValues region grid = nub $ concat combs
  where
    knownValues = regionValues region grid
    targetLength = length (squares region) - length knownValues
    targetSum = total region - sum knownValues
    possibleValues = permittedValues \\ knownValues
    combs = combinations targetLength targetSum possibleValues

regionValues :: Region -> Grid -> [Int]
regionValues region grid = filter (/= emptySquare) values
  where
    values = map (`valueAt` grid) $ squares region

combinations :: Int -> Int -> [Int] -> [[Int]]
combinations 1 targetSum xs = [[targetSum] | targetSum `elem` xs]
combinations targetLength targetSum xs = concatMap combinationsAt [0 .. length xs - targetLength]
  where
    combinationsAt n = let (y:ys) = drop n xs in
      map (y:) $ combinations (targetLength-1) (targetSum-y) ys

regions :: [String] -> [(Char, Int)] -> [Region]
regions pattern totals = sortOn valCount $ map makeRegion (nub $ concat pattern)
  where
    valCount r = length $ possibleRegionValues r emptyGrid
    makeRegion x = Region (squares x) (total x)
    squares x = squaresContaining x pattern
    total x = fromJust $ lookup x totals

toPuzzle :: [String] -> [(Char, Int)] -> Puzzle
toPuzzle pattern totals = KillerPuzzle (regions pattern totals) emptyGrid

-- Weekly 935
killerPuzzle :: Puzzle
killerPuzzle = toPuzzle 
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
