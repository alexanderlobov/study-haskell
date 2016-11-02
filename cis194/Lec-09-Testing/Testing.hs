module Mine where

import Test.QuickCheck
import Data.List

type MergeFun = [Integer] -> [Integer] -> [Integer]

merge1 :: Ord a => [a] -> [a] -> [a]
merge1 (x:xs) (y:ys)
    | x < y = x : merge1 xs ys
    | otherwise = y : merge1 xs ys
merge1 _ _ = []

merge2 :: MergeFun
merge2 all_xs@(x:xs) all_ys@(y:ys)
    | x < y = x : merge2 xs all_ys
    | otherwise = y : merge2 all_xs ys
merge2 xs ys = xs ++ ys


prop_numElements :: MergeFun -> [Integer] -> [Integer] -> Bool
prop_numElements merge xs ys
    = length xs + length ys == length (merge xs ys)

prop_sorted :: MergeFun -> OrderedList Integer -> OrderedList Integer -> Bool
prop_sorted merge (Ordered xs) (Ordered ys)
    = merge xs ys == sort (xs ++ ys)

isSorted :: Ord a => [a] -> Bool
isSorted (a:b:rest) = a <= b && isSorted (b:rest)
isSorted _ = True
