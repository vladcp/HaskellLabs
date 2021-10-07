module RecursionLecture where

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort list = merge(mergesort xs) (mergesort ys) where
    (xs,ys) = split1 list

split1 :: [a] -> ([a],[a])
split1 [] = ([],[])
split1 [x]= ([],[x])
split1 (x:y:zs) = (x:xs, y:ys) where
    (xs, ys) = split1 zs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x:(merge xs(y:ys))
    | otherwise = y: (merge (x:xs) ys)


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y<=x] ++ [x] ++ 
    qsort [z | z <- xs, z>x]

-- replace a substring with another substring
replace :: String -> String -> String -> String 
replace orig new [] = []
replace orig new (x:xs) 
    | orig == prefix = new ++ replace orig new rest 
    | otherwise = x : replace orig new xs
        where(prefix, rest) = splitAt(length orig) (x:xs)

--  generate list of primes
primes :: [Int]
primes = [x | x<-[2..], prime x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n`mod`x == 0]
prime n = factors n == [1,n]

-- different primes def
primes' :: [Int]
primes' = sieve [2..]
    where 
        sieve(p:xs) = p : sieve [n | n <- xs, n `mod` p > 0]