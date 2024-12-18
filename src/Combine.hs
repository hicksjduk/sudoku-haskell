{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Combine where

combineMinMaxLength :: Int -> Int -> [a] -> [[a]]
combineMinMaxLength 0 max xs = [] : combineMinMaxLength 1 max xs
combineMinMaxLength _ _ [] = []
combineMinMaxLength _ 0 _ = []
combineMinMaxLength min max xs = 
    let combineAt n = (xs !! n :) <$> combineMinMaxLength (min - 1) (max - 1) (drop (n + 1) xs)
    in concatMap combineAt $ take (length xs) [0 ..]

combineAll :: [a] -> [[a]]
combineAll xs = combineMinMaxLength 1 (length xs) xs

combineMinLength :: Int -> [a] -> [[a]]
combineMinLength min xs = combineMinMaxLength min (length xs) xs

combineMaxLength :: Int -> [a] -> [[a]]
combineMaxLength = combineMinMaxLength 1

combineExactLength :: Int -> [a] -> [[a]]
combineExactLength len = combineMinMaxLength len len
