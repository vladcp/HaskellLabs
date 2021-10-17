module Bag where

import Data.List (sortBy)
data Bag a = Bag [(a, Int)] deriving (Show,Eq,Ord)

--turns list into bag that contains number of occurences
listToBag :: Eq a => [a] -> Bag a 
listToBag as = Bag xs where
    xs = sortBy compare (uniq [][(a, (count a as)) | a <- as])

--counts the number of occurences of an item in list
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

--uniq removes duplicates from a list, returns list with each item occuring once
uniq :: Ord a => [a] -> [a] -> [a]
uniq x [] = x 
uniq [] (a:xs) = uniq [a] xs
uniq x (a:xs) = if a `elem` x then uniq x xs else uniq (a:x) xs

bagEqual :: Ord a => Bag a -> Bag a -> Bool
bagEqual (Bag a) (Bag b) = a==b

-- bagInsert :: a -> Bag a -> Bag a
-- bagInsert a (Bag b) = Bag (a:b)


