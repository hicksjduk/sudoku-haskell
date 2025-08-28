{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Sudoku where

import Data.List
import Data.Maybe
import Data.Function
import Data.Functor
import Combine
import Data.Ord
import Control.Parallel

permittedValues :: [Int]
permittedValues = [1..9]
emptySquare :: Int
emptySquare = 0
gridSize :: Int
gridSize = length permittedValues

data Puzzle = Puzzle [[Dimension]] Grid deriving (Eq, Show)

type Grid = [[Int]]

type Square = (Int, Int)

type IntRange = (Int, Int)

type Box = (IntRange, IntRange)

data Dimension = Dimension [Square] [[Int]] DimensionType deriving (Eq, Show)

instance Ord Dimension where
  compare =
    let criteria = [length . emptySquares, length . possibleValues]
    in foldMap (compare `on`) criteria

data DimensionType = RowD Int | ColumnD Int | BoxD Box | RegionD deriving (Eq, Show)

data SquareData = SquareData Square [DimensionData] deriving (Show)

data DimensionData = DimensionData Dimension ValueRemover

instance Show DimensionData where
  show (DimensionData d _) = show d

type ValueRemover = (Int -> [Dimension])

sudoku :: Puzzle -> Either String Grid
sudoku p = if parametersValid then solveIt =<< validate p else Left "Invalid"
  where
    solveIt p = case solve p of
      [] -> Left "No solution found"
      (solution:_) -> Right solution

solve :: Puzzle -> [Grid]
solve p@(Puzzle _ grid) = case emptySquareData p of
  [] -> [grid]
  (square:_) -> solveAt square p

solveAt :: SquareData -> Puzzle -> [Grid]
solveAt square p = solveParallel $ allowedValues square
  where
    solveParallel [] = []
    solveParallel vv@(v:vs)
      | length vv > chunkSize = let solveOthers = solveParallel vs
          in par solveOthers $ solveUsing v ++ solveOthers
      | otherwise = concatMap solveUsing vv
    solveUsing value = solve $ withValueAt square value p
    chunkSize = 3

emptySquareData :: Puzzle -> [SquareData]
emptySquareData (Puzzle dimsByType _) = map squareData emptySq
  where
    squareData sq = SquareData sq $ containingDimensionData sq dimsByType
    emptySq = concatMap emptySquares $ sort $ map head $ filter (not . null) dimsByType

emptySquares :: Dimension -> [Square]
emptySquares (Dimension sq _ _) = sq

containingDimensionData :: Square -> [[Dimension]] -> [DimensionData]
containingDimensionData sq dimsByType = containingDims
  where
    containingDims = mapMaybe (find containsSquare . dimData) dimsByType
    dimData dimensionsOfType = zipWith (dimensionData dimensionsOfType) dimensionsOfType [0..]
    dimensionData dims dim index = DimensionData dim $ removeValue dims index sq
    containsSquare (DimensionData dim _) = sq `inDimension` dim

allowedValues :: SquareData -> [Int]
allowedValues (SquareData _ dims) =
  let possibles (DimensionData d _) = possibleValues d
  in foldr1 intersect $ map possibles dims

possibleValues :: Dimension -> [Int]
possibleValues (Dimension _ [vals] _) = vals
possibleValues (Dimension _ combs _) = nub $ concat combs

withValueAt :: SquareData -> Int -> Puzzle -> Puzzle
withValueAt (SquareData sq sqDims) v (Puzzle _ g) = Puzzle newDims $ setValueAt sq v g
  where
    newDims = map updateDims sqDims
    updateDims (DimensionData _ valueRemover) = valueRemover v

setValueAt :: Square -> Int -> Grid -> Grid
setValueAt (row, col) value grid =
  let newRow = replaceValueAt col value (grid !! row)
    in replaceValueAt row newRow grid

replaceValueAt :: Int -> a -> [a] -> [a]
replaceValueAt index value xs = let (before, _:after) = splitAt index xs
  in before ++ value : after

withoutValueAt :: Square -> Int -> Dimension -> Maybe Dimension
withoutValueAt _ _ (Dimension [_] _ _) = Nothing
withoutValueAt sq v (Dimension emptySquares combs dimType) =
  Just $ Dimension newSq newCombs dimType
  where
    newSq = delete sq emptySquares
    newCombs = mapMaybe (deleteIfPresent v) combs

removeValue :: [Dimension] -> Int -> Square -> Int -> [Dimension]
removeValue dims index sq value = case (index, newDim) of
  (0, _) -> newDims
  (_, Nothing) -> newDims
  _ -> let (left, right) = splitAt (index + 1) newDims in sort left ++ right
  where
    newDim = withoutValueAt sq value $ dims !! index
    newDims = replaceOrDeleteValueAt index newDim dims

replaceOrDeleteValueAt :: Int -> Maybe a -> [a] -> [a]
replaceOrDeleteValueAt i v xs =
  let (before, _:after) = splitAt i xs
  in before ++ maybe after (:after) v

deleteIfPresent :: (Eq a) => a -> [a] -> Maybe [a]
deleteIfPresent x xs = elemIndex x xs <&> (`deleteAt` xs)
  where
    deleteAt i xs = let (before, _:after) = splitAt i xs
      in before ++ after

inDimension :: Square -> Dimension -> Bool
inDimension sq (Dimension _ _ (RowD row)) = sq `inRow` row
inDimension sq (Dimension _ _ (ColumnD col)) = sq `inColumn` col
inDimension sq (Dimension _ _ (BoxD box)) = sq `inBox` box
inDimension sq (Dimension emptySquares _ RegionD) = sq `elem` emptySquares

inRow :: Square -> Int -> Bool
inRow (row, _) = (== row)

inColumn :: Square -> Int -> Bool
inColumn (_, col) = (== col)

inBox :: Square -> Box -> Bool
(row, col) `inBox` (rows, cols) = inRange row rows && inRange col cols

inRange :: Int -> IntRange -> Bool
inRange v (minVal, maxVal) = v >= minVal && v <= maxVal

dimension :: [Square] -> Grid -> DimensionType -> Dimension
dimension squares grid = Dimension emptySquares [comb]
  where
    valuesBySquare = zip squares $ map (`valueAt` grid) squares
    (empty, notEmpty) = partition ((== emptySquare) . snd) valuesBySquare
    emptySquares = map fst empty
    comb = permittedValues \\ map snd notEmpty

valueAt :: Square -> Grid -> Int
valueAt (row, col) grid = grid !! row !! col

rowDimension :: Int -> Grid -> Dimension
rowDimension row grid =
  let sq = map (row,) $ take gridSize [0..]
  in dimension sq grid $ RowD row

columnDimension :: Int -> Grid -> Dimension
columnDimension col grid =
  let sq = map (, col) $ take gridSize [0..]
  in dimension sq grid $ ColumnD col

boxDimension :: Box -> Grid -> Dimension
boxDimension box@((minRow, maxRow), (minCol, maxCol)) grid =
  let sq = [(r, c) | r <- [minRow .. maxRow], c <- [minCol .. maxCol]]
  in dimension sq grid $ BoxD box

standardDimensions :: Grid -> [[Dimension]]
standardDimensions grid = [rowDims, colDims, boxDims]
  where
    rowDims = map (`rowDimension` grid) $ take gridSize [0..]
    colDims = map (`columnDimension` grid) $ take gridSize [0..]
    boxDims = map (`boxDimension` grid) $ boxes

puzzleFrom :: [[Dimension]] -> Grid -> Puzzle
puzzleFrom dims = Puzzle $ map sort dims

sudokuPuzzle :: Grid -> Puzzle
sudokuPuzzle grid = puzzleFrom (standardDimensions grid) grid

killerPuzzle :: [(String, [Int])] -> Puzzle
killerPuzzle puzzleDef = Puzzle (regionDimensions : standardDimensions emptyGrid) emptyGrid
  where
    pattern = let patternRow (p, _) = p in map patternRow puzzleDef
    totals = let totalsInRow (_, t) = t in concatMap totalsInRow puzzleDef
    sqLists = map (`squaresContaining` pattern) $ nub $ concat pattern
    regionDimensions = sort $ zipWith makeRegion sqLists totals
    makeRegion sqs tot = Dimension sqs (combinations (length sqs) tot) RegionD

emptyGrid :: Grid
emptyGrid = replicate gridSize $ replicate gridSize emptySquare

squaresContaining :: Eq a => a -> [[a]] -> [Square]
squaresContaining v grid =
  let squares rowIndex colValues = zip (repeat rowIndex) $ elemIndices v colValues
  in concat $ zipWith squares [0..] grid

combinations :: Int -> Int -> [[Int]]
combinations size total = cache !! size !! total
  where
    cache = map cacheBy [0..]
    cacheBy s = map (combs s) [0..]
    combs s t = filter ((== t) . sum) $ combineExactLength s permittedValues

parametersValid :: Bool
parametersValid
  | gridSize == 0 =
    error "Grid size is 0"
  | hasDuplicates permittedValues =
    error "Permitted values include duplicate(s)"
  | emptySquare `elem` permittedValues =
    error "Empty square value is also a permitted value"
  | otherwise = True

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

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

validate :: Puzzle -> Either String Puzzle
validate p@(Puzzle dimsByType grid) =
  let regionDims = concat $ filter (isRegion . head) dimsByType
  in validateGrid grid >>= validateRegions regionDims >> return p

isRegion :: Dimension -> Bool
isRegion (Dimension _ _ RegionD) = True
isRegion Dimension {} = False

validateGrid :: Grid -> Either String Grid
validateGrid grid
  | length grid /= gridSize =
    Left "Wrong number of rows"
  | any ((/= gridSize) . length) grid =
    Left "Wrong number of columns"
  | any (any (`notElem` emptySquare : permittedValues)) grid =
    Left "Invalid cell value"
  | any (hasDuplicates . rowValues) indices =
    Left "Row contains duplicate value(s)"
  | any (hasDuplicates . colValues) indices =
    Left "Column contains duplicate value(s)"
  | any (hasDuplicates . boxValues) boxes =
    Left "Box contains duplicate value(s)"
  | otherwise = return grid
  where
    indices = take gridSize [0..]
    rowValues row = filter (/= emptySquare) $ grid !! row
    colValues col = filter (/= emptySquare) $ map (!! col) grid
    boxValues (rows, cols) = filter (/= emptySquare) values
      where
        values = concatMap (slice cols) $ slice rows grid
        slice (x, y) xs = drop x $ take (y-1) xs

validateRegions :: [Dimension] -> Grid -> Either String [Dimension]
validateRegions [] _ = return []
validateRegions ds grid
  | any totalOutOfRange ds =
    Left "Region total out of permitted range"
  | sum (map total ds) /= (gridSize * sum permittedValues) - sum (map sum grid) =
    Left "Region totals incorrect"
  | any sizeOutOfRange ds =
    Left "Incorrect number of squares in regions"
  | length sq /= length emptySq =
    Left "Incorrect number of squares in regions"
  | not (all (`elem` emptySq) sq) =
    Left "Square in region is not empty"
  | any indexOutOfRange sq =
    Left "Square index out of range"
  | otherwise = return ds
  where
    sq = concatMap emptySquares ds
    emptySq = squaresContaining emptySquare grid
    indexOutOfRange (row, col) = outOfRange row || outOfRange col
    outOfRange n = n < 0 || n >= gridSize
    sizeOutOfRange d = let len = size d
      in len < 1 || len > gridSize

size :: Dimension -> Int
size (Dimension sq _ _) = length sq

total :: Dimension -> Int
total (Dimension _ (c:_) _) = sum c

minRegionTotal :: Int -> Int
minRegionTotal count = (sum . take count . sort) permittedValues

maxRegionTotal :: Int -> Int
maxRegionTotal count = (sum . take count . sortBy (comparing Data.Ord.Down)) permittedValues

totalOutOfRange :: Dimension -> Bool
totalOutOfRange d = let count = size d
  in not $ inRange (total d) (minRegionTotal count, maxRegionTotal count)

puzzle :: Puzzle
puzzle = sudokuPuzzle
         [[8,0,0,0,0,0,0,0,0],
          [0,0,3,6,0,0,0,0,0],
          [0,7,0,0,9,0,2,0,0],
          [0,5,0,0,0,7,0,0,0],
          [0,0,0,0,4,5,7,0,0],
          [0,0,0,1,0,0,0,3,0],
          [0,0,1,0,0,0,0,6,8],
          [0,0,8,5,0,0,0,1,0],
          [0,9,0,0,0,0,4,0,0]]

-- Weekly 935
killer1 :: Puzzle
killer1 = killerPuzzle
  [
    ("aabbcddee", [14, 8, 44, 15, 12]),
    ("affcccgge", [29, 26]),
    ("ffcchccgg", [4]),
    ("fijjhjjkg", [25, 36, 17]),
    ("iiljjjmkk", [10, 7]),
    ("inlooompk", [25, 45, 35]),
    ("inooqoopk", [18]),
    ("nnorqsopp", [9, 10]),
    ("nttrqsuup", [12, 4])
  ]

-- Daily 6617
killer2 :: Puzzle
killer2 = killerPuzzle
  [
    ("aabbcddef", [14, 8, 16, 10, 9, 13]),
    ("gghcciief", [7, 11, 9]),
    ("jjhklmnno", [10, 9, 10, 12, 11, 7]),
    ("pqqklmrro", [8, 10, 11]),
    ("psstttuuv", [12, 11, 16, 4]),
    ("wxxyz122v", [10, 9, 17, 6, 7, 12]),
    ("w33yz1455", [9, 11, 8]),
    ("6788994AA", [16, 10, 5, 19, 11]),
    ("67BB9CCDD", [11, 4, 12])
  ]
