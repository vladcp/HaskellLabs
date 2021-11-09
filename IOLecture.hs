module IOLecture where
  import Control.Exception
  import System.IO
  import System.IO.Error
    
sayHello :: IO()
sayHello = putStrLn "Hello World"


-- 'do' alows sequences inside IO function
greeting :: IO()
greeting = do
    putStrLn "Greetings! What is your name?"

    -- backward arrow makes it just a  string, not IO string
    inpStr <- getLine --reads a line, returns string (type IO string)
    putStrLn ("Welcome to Haskell, " ++ inpStr ++ "!")

getLine' :: IO String
getLine' = do 
        x <- getChar
        if x == '\n' then
            return []
        else do
            xs <- getLine'
            return (x:xs)

inh <- openFile "input.txt" ReadMode
inpStr <- hGetLine inh