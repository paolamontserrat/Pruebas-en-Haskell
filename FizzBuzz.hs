module FizzBuzz (someFunc, fizzbuzz, lessThan20, tens, number) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

ifThenElse :: Bool -> a -> a -> a
ifThenElse cond thenVal elseVal =
    case cond of
        True -> thenVal
        False -> elseVal   

 

fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 15 == 0 = "fizzbuzz!"
    | n `mod` 3 == 0  = "fizz!"
    | n `mod` 5 == 0  = "buzz!"
    | otherwise       = number n ++ "!"



lessThan20 :: Int -> String           
lessThan20 n
    | n > 0 && n < 20 = answers !! (n - 1)
    | otherwise = "out of range"
    where
        answers = words "one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"

tens :: Int -> String
tens n 
    | n >= 2 && n <= 9 = answers !! (n - 2)
    | otherwise = "out of range"
    where
        answers = words "twenty thirty forty fifty sixty seventy eighty ninety"

number :: Int -> String
number n 
    | n > 0 && n < 20 = lessThan20 n
    | n `mod` 10 == 0 && n < 100  = tens (n `div` 10)
    | n < 100 = tens (n `div` 10) ++ " " ++ lessThan20 (n `mod` 10)
    | n == 100 = "one hundred"

