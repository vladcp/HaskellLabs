module MergeSort where

import Data.List
import Data.String
import Data.Char

halves :: [a] -> ([a],[a])
halves xs = splitAt n xs where
    n = length xs `div` 2

--alternative implementation
halves' :: [a] -> ([a], [a])
halves' xs = (take n xs, drop n xs) where
    n = length xs `div` 2