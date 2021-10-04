module Lists where
import Data.List
import Data.String
import Data.Char

--converts letter to index
let2int :: Char -> Int
let2int c = ord c - ord 'a'

--converts index to letter
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

--shifts a character by an int amount
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

--shifts every character of string to right
encode :: Int -> String -> String
encode n str = [shift n x | x <- str] 

--shifts every character of string to left
decode :: Int -> String -> String 
decode n str = [shift (-n) x | x <- str]