module Bag where

import Data.List 
data Bag a = Bag [(a, Int)] deriving (Show,Eq,Ord)

--turns list into bag that contains number of occurences
listToBag :: Ord a => [a] -> Bag a 
listToBag as = Bag xs where
    xs = (uniq [][(a, (count a as)) | a <- as])

--counts the number of occurences of an item in list
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

--uniq removes duplicates from a list, returns list with each item occuring once
--same order of items
uniq :: Ord a => [a] -> [a] -> [a]
uniq x [] = x 
uniq [] (a:xs) = uniq [a] xs
uniq x (a:xs) = if a `elem` x then uniq x xs else uniq (x ++ [a]) xs

--checks if two bags are equal
--basically checks that the lists contained by the bags are equal
--order doesnt matter
bagEqual :: Ord a => Bag a -> Bag a -> Bool
bagEqual (Bag as) (Bag bs) = equalLists as bs

--checks if a list is a sublist of another
contains :: Eq a => [a] -> [a] -> Bool
contains [] ys = True
contains (x:xs) ys = (x `elem` ys) && contains xs ys

--checks if two lists are sublists of each other
--aka they contain the same elements
equalLists :: Ord a => [a] -> [a] -> Bool
equalLists xs ys = contains xs ys && contains ys xs

-- inserts item in bag or increases number of occurences if it exists
bagInsert :: Eq a => a -> Bag a -> Bag a
bagInsert a (Bag lst) = Bag (itemInsert a lst)

--returns list with all the elements in the bag
elementsInBag :: Bag a -> [a]
elementsInBag (Bag a) = fst (unzip a)

--inserts item in a list that is contained by bag
itemInsert :: Eq a => a -> [(a,Int)] -> [(a,Int)]
itemInsert a lst =
    let 
        as = fst (unzip lst)
        index = func (elemIndex a as)
        (x ,(y:ys)) = splitAt index lst --y is the tuple that contains a
        n = (snd y) + 1 --increase number of as in the tuple by 1
    in
        --if a is in the list
        if index > -1 then 
        x ++ (a,n):ys --put it all back
        else 
        --insert a at the beginning of list
        (a,1):lst

--inserts an item that has a number of occurences
elementInsert :: Eq a => (a,Int) -> [(a,Int)] -> [(a,Int)]
elementInsert (e,ne) lst =
    let 
        as = fst (unzip lst)
        index = func (elemIndex e as)
        (x,(y:ys)) = splitAt index lst --y is the tuple that contains a
        n = (snd y) + ne --increase number of as in the tuple by 1
    in
        --if a is in the list
        if index > -1 then 
        x ++ (e,n):ys --put it all back
        else 
        --insert a at the beginning of list
        (e,ne):lst

func :: Maybe Int -> Int
func (Just i) = i
func Nothing = -1

--bagsum
bagSum :: Eq a => Bag a -> Bag a -> Bag a
bagSum b (Bag []) = b
--insert every element from bag 2 to bag 1
bagSum (Bag b1) (Bag (b:bs)) = bagSum (Bag(elementInsert b b1)) (Bag bs)

--bagintersection
-- bagIntersection :: Eq a => [a] -> Bag a -> Bag a -> [a]
-- bagIntersection [] b1 b2 =
--     let 
--         itemsB1 = elementsInBag b1
--         itemsB2 = elementsInBag b2
--         intersectedItems = intersect itemsB1 itemsB2
--     in


--return occurences of an item in a list like bag
getOccurences :: Eq a => a -> [(a,Int)] -> Int
getOccurences a = snd.head.filter (\(x,y) -> x == a)

chooseItLeastOc :: Eq a =>[a] -> Bag a -> Bag a -> [(a,Int)]
chooseItLeastOc (a:as) (Bag l1) (Bag l2) =
     let
         n = getOccurences a (elementsInBag l1) 
         m = getOccurences a (elementsInBag l2)
     in
         [(a, min m n)] ++ chooseItLeastOc as (Bag l1) (Bag l2)