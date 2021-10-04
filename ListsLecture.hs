module Lists where
import Data.List
import Data.String
import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n str = [shift n x | x <- str] 

decode :: Int -> String -> String 
decode n str = [shift (-n) x | x <- str]