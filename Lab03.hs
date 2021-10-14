module Lab03 where
import GHC.Classes (Ord)

data Shape = Circle Float | Rect Float Float 
    deriving (Show, Eq)

--exercise 1
--scale scales up an item of type shape by a certain amount
scale :: Float -> Shape -> Shape
scale n (Circle x) = Circle (n*x)
scale n (Rect x y) = Rect (n*x) (y*n)

--exercise 2 - TREES
--function that outputs if a binary tree is complete

--define a binary tree type
data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving (Show)

complete :: Tree a -> Bool
complete Empty = True 
complete (Node (left) var (right)) = height left == height right && complete left && complete right

height :: Tree a -> Int
height Empty = 0
height (Node (left) val (right)) = 1 + maximum[(height left), (height right)]

test_tree = Node(Node Empty 5 Empty) 6 (Node(Node Empty 4 Empty) 7  Empty)
test_tree2 = Node(Node Empty 5 Empty) 6 (Node Empty 7 Empty)

--exercise 4 - compare shapes based on perimeter rather than area
instance Ord Shape where
    compare (Circle r1) (Circle r2) = compare r1 r2
    compare (Rect a b) (Rect c d) = compare (a+b) (c+d)

method :: Shape -> Shape -> Bool
method (Rect a b) (Rect c d)  = if (Rect a b) > (Rect c d) then True else False

--exercise 5
-- [f x | x <- xs, p x]
funcMap ::[Int] -> [Int]
funcMap xs = map f (filter (p) xs) 
    where
     p x = x > 3
     f x = 0 --just examples

--exercise 6 - redefine map, filter using foldr
map' f = foldr g [] where
    g y ys = (f y):ys

filter' f = foldr g [] where
    g x xs | f x = x:xs
           | otherwise = xs

--exercise 7
mergesort :: (a->a->Bool) -> [a] -> [a]
mergesort cmp [] = []
mergesort cmp [x] = [x]
mergesort cmp xs
  = merge cmp (mergesort cmp ys) (mergesort cmp zs)
    where (ys, zs) = halves xs

--cmp is  a function that compares two elements and checks if they are in the right order
--cmp dictates what kind of items we are sorting and in what kind of order

merge :: (a->a->Bool) -> [a] -> [a] -> [a]
merge cmp [] ys = ys
merge cmp xs [] = xs
merge cmp (x:xs) (y:ys)
    | cmp x y = x:(merge cmp xs (y:ys))
    | otherwise = y: (merge cmp (x:xs) ys)

cmp :: Ord a => a -> a -> Bool
cmp a b | a <= b = True
        | otherwise = False

--for exercise 7
cmpTuples :: Ord a => (String, a) -> (String, a) -> Bool
cmpTuples a b | snd a <= snd b = True
              | otherwise = False

halves :: [a] -> ([a], [a])
halves xs = splitAt a xs where
        a = length  xs `div` 2

--exercise 8
--maximum using fold functions
-- maximum' = foldr g [] where
--     g x xs = 

maximum' = foldr fun [] where
    fun x xs | [x] == xs = x
           | x > maximum' xs = x
           | otherwise = maximum' xs

max' [x] = x
max' (x:xs) | x > max' xs = x
            | otherwise = max' xs