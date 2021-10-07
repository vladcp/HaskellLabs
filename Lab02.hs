module Lab02 where
--exercise 1
--rewriting list comprehension
func :: Int -> [Int]
func t = map f (filter even [1..20]) where
    f x = x ^ 2

--exercise 2
--find pythagorean triplets up to a number
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a,b,c) | a <- [1..n], b<-[1..n], c<-[1..n], a^2 + b^2 == c^2]

--exercise 3
--return list of indices of all instances of x
-- indices :: a -> [a] -> [a]
indices x ls = [n | (y,n) <- zip ls [0..], x == y]

--TODO do recursively

--exercise 4
funct lst = (\(_:lst) -> lst)

--exercise 5
--lambda expressions
increment = (\x -> x + 1) 
decrement = (\x -> x - 1) 



--exercise 6
--
records = [(True,5), (False,7), (True,12), (True,8), (False,15), (True,4)]
choose :: (Ord a, Num a) => [(Bool, a)] -> [(Bool, a)]
choose records = [(x,y) | (x,y) <- records, x, y < 10 ]


--exercise 7
-- Luhn Algorithm
luhnDouble :: Int -> Int 
luhnDouble x = if x*2 > 9 then x*2 - 9 else x * 2

luhn :: Int -> Int -> Int -> Int -> Bool 
luhn a b c d = result `mod` 10 == 0 where
    result = luhnDouble a + b + luhnDouble c + d

-- exercise 8
-- factorial for negative - error
fact :: Int -> Int 
fact 0 = 1
fact n 
    | n >= 0 = n * fact (n-1) 
    | otherwise = error "Undefined for negative numbers"

--exercise 9
nth :: Int -> [a] -> a 
--base cases: empty list, or 0th item
nth n [] = error "Insufficient items"
nth 0 (x:xs) = x 
nth n (x:xs) = nth (n-1) xs

--exercise 10 
-- Euclid's algorithm, gcd
euclid :: Int -> Int -> Int 
euclid x y 
    | x==y = x
    | x > y = euclid (x-y) y
    | otherwise = euclid x (y-x)

--exercise 11
-- sum of list of numbers
sumlist :: [Int] -> Int 
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

-- take n elements from start of list 
takefromlist :: Int -> [a] -> [a]
takefromlist 0 xs = []
takefromlist n [] = error "Not enough elements in list"
takefromlist n (x:xs) = x : takefromlist (n-1) xs 

-- select last element of a non-empty list
lastelement :: [a] -> a
lastelement [] = error "No items in list"
lastelement [x] = x
lastelement (x:xs) = lastelement(xs)

--exercise 12
zip' :: [a] -> [b] -> [(a, b)]
zip' [][] = []
zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

-- --exercise 13
studentsgrades :: [(String, Int)] -> (Int, Int, Int, Int, Int, Int)
studentsgrades ls = (a,b,c,d,e,f) where
    a = length (filter (<30) grades)
    b = length (filter (\x -> x>=30 && x<40) grades)
    c = length (filter (\x -> x>=40 && x<50) grades)
    d = length (filter (\x -> x>=50 && x<60) grades)
    e = length (filter (\x -> x>=60 && x<70) grades)
    f = length (filter (>=70) grades)
    grades = map snd ls

--exercise 14
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--exercise 15
factors :: Int -> [Int]
factors x = [y | y <- [1..x-1], x `mod` y == 0]

perfects :: Int -> [Int]
perfects x = [n | n <- [1..x], n == sum (factors n)]

--exercise 16
--exercise 17
approx_pi :: Float -> (Float, Int)
approx_pi x =
