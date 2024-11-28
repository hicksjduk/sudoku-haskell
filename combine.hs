module Combine where

import Data.List

combineMinMaxLength :: (Eq a) => Int -> Int -> [a] -> [[a]]
combineMinMaxLength 0 max xs = [] : combineMinMaxLength 1 max xs
combineMinMaxLength _ _ [] = []
combineMinMaxLength _ 0 _ = []
combineMinMaxLength min max xs = 
    let combineAt n = (xs !! n :) <$> combineMinMaxLength (min - 1) (max - 1) (drop (n + 1) xs)
    in concatMap combineAt $ take (length xs) [0 ..]

combineAll :: (Eq a) => [a] -> [[a]]
combineAll xs = combineMinMaxLength 1 (length xs) xs

combineMinLength :: (Eq a) => Int -> [a] -> [[a]]
combineMinLength min xs = combineMinMaxLength min (length xs) xs

combineMaxLength :: (Eq a) => Int -> [a] -> [[a]]
combineMaxLength = combineMinMaxLength 1

combineExactLength :: (Eq a) => Int -> [a] -> [[a]]
combineExactLength len = combineMinMaxLength len len
