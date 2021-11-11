module MonadsLecture where

-- IO and Maybe are Monads, implement all four functions previosly mentioned
-- ====== bind (>>=), then (>>), return, fail ======= --

maybeHalf :: Int -> Maybe Int
maybeHalf a 
    | even a = Just (div a 2)
    | otherwise = Nothing
    -- try: Just 10 >>= maybeHalf
    -- ">>=" - "bound to"

-- ">>" - "then" operator - it does nothing with the value contained within the Monad
-- do is shorthand 
main = putStr "What is your name?"
        >> readLn
        >>= \a -> putStr "How old are you?"
        >>readLn 
        >>= \b -> putStrLn (a,b)

-- main = do putStr "What is your name"
--     a <- readLn
--     putStr "How old are you"
--     b <- print

-- return and fail --
-- return takes a normal value and puts it in a context

--there is no standard fc for extracting value from Monad
-- Maybe provides fromJust

--can do:
getVal (Just x) = x