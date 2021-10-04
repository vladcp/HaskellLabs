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

--exercise 4
--return list of indices of all instances of x
-- indices :: a -> [a] -> [a]
indices x ls = [n | (y,n) <- zip ls [0..], x == y]

--exercise 4
funct lst = (\(_:lst) ->lst)

--exercise 5
--lambda expressions
increment = (\x -> x + 1) 
decrement = (\x -> x - 1) 

isPrime k = if k > 1 then null [ x | x <- [2..sqrt k], k `mod` x == 0] else False

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
