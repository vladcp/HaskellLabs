module Test where
import Data.List
import Data.String
import Data.Char

fn :: Int -> [Int] -> Bool 
fn a bs = let
           x = minimum bs
           y = maximum bs
           in
               sum(bs \\ [x,y]) > a

-- returns a tuple of the two halfs of a list
halve :: [Int] -> ([Int], [Int])
halve xs = let
            a = length  xs `div` 2
            in
                splitAt a xs

-- eucledian :: (Float, Float) -> (Float, Float) -> Float  
-- calculates euclidean distance between two points
eucledian (x1,y1) (x2,y2) = dist where
    sum = (x2 - x1)^2 + (y2 - y1)^2
    dist = sqrt sum

-- return first word in a string

firstword :: String -> String
firstword xs = head (words xs)

safetail :: [Char] -> [Char]
safetail xs = if null xs then [] else tail xs

safetail' :: [Char] -> [Char]
safetail' xs | null xs = []
             | otherwise = tail xs

--stack takes first element of list, puts it at the end of list
stack :: [a] -> [a]
stack xs = result where
    headlist = [head xs]
    taillist = tail xs
    result = taillist ++ headlist
--range takes numerical value, checks if it's between 0 and 10
range :: Float -> Bool
range x | x < 0 || x > 10 = False
        | otherwise = True

--addc adds a Char to the beginning of String
addc :: Char -> String -> String
addc chr str = result where
    chrlist = [chr]     --turn char into list [char]
    result = chrlist ++ str   -- concatenate the char and string (both are lists now)

--divides each item of the list by 2
halves :: [Float] -> [Float]
halves xs = map (\x -> x/2) xs 
 -- or map f xs where f x = x/2

--capitalize first letter of string
capitalizeStart :: String -> String
capitalizeStart (head:tail) = toUpper head : tail
capitalizeStart [] = []

--returns odd items in a list
oddItems :: [Int] -> [Int]
oddItems lst = filter odd lst where
    




