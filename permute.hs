module Permute where

import Data.List

permuteMinMaxLength :: (Eq a) => Int -> Int -> [a] -> [[a]]
permuteMinMaxLength 0 max xs = [] : permuteMinMaxLength 1 max xs
permuteMinMaxLength _ 0 _ = []
permuteMinMaxLength min max xs = 
  let permuteUsing x = (x :) <$> permuteMinMaxLength (min - 1) (max - 1) (delete x xs)
  in concatMap permuteUsing $ nub xs

permuteAll :: (Eq a) => [a] -> [[a]]
permuteAll xs = permuteMinMaxLength 1 (length xs) xs

permuteMinLength :: (Eq a) => Int -> [a] -> [[a]]
permuteMinLength min xs = permuteMinMaxLength min (length xs) xs

permuteMaxLength :: (Eq a) => Int -> [a] -> [[a]]
permuteMaxLength = permuteMinMaxLength 1

permuteExactLength :: (Eq a) => Int -> [a] -> [[a]]
permuteExactLength len = permuteMinMaxLength len len
