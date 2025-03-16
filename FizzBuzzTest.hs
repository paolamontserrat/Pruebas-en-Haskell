module Main where
-- Paola Montserrat Ruiz Carmen y America Citlalli Lopez Lemus
import Test.Tasty
import Test.Tasty.HUnit
import FizzBuzz

lessThan20Answers = words ("one two three four five six seven eight nine ten " ++
                            "eleven twelve thirteen fourteen fifteen sixteen " ++
                            "seventeen eighteen nineteen")

tensAnswers = words "twenty thirty forty fifty sixty seventy eighty ninety"

fizzBuzzSuite :: TestTree
fizzBuzzSuite = testGroup "FizzBuzz tests"
                    [ testGroup "fizzbuzz" $
                        [ testCase "1 is one!" $ fizzbuzz 1 @?= "one!"
                        , testCase "2 is two!" $ fizzbuzz 2 @?= "two!"
                        , testCase "3 is fizz!" $ fizzbuzz 3 @?= "fizz!"
                        , testCase "4 is four!" $ fizzbuzz 4 @?= "four!"
                        , testCase "5 is buzz!" $ fizzbuzz 5 @?= "buzz!"
                        , testCase "6 is fizz!" $ fizzbuzz 6 @?= "fizz!"
                        , testCase "10 is buzz!" $ fizzbuzz 10 @?= "buzz!"
                        , testCase "15 is fizzbuzz!" $ fizzbuzz 15 @?= "fizzbuzz!"
                        , testCase "18 is fizz!" $ fizzbuzz 18 @?= "fizz!"
                        , testCase "20 is buzz!" $ fizzbuzz 20 @?= "buzz!"
                        , testCase "22 is twenty two!" $ fizzbuzz 22 @?= "twenty two!"
                        , testCase "25 is buzz!" $ fizzbuzz 25 @?= "buzz!"
                        , testCase "30 is fizzbuzz!" $ fizzbuzz 30 @?= "fizzbuzz!"
                        , testCase "60 is fizzbuzz!" $ fizzbuzz 60 @?= "fizzbuzz!"
                        , testCase "99 is fizz!" $ fizzbuzz 99 @?= "fizz!"
                        , testCase "100 is buzz!" $ fizzbuzz 100 @?= "buzz!"
                        ]
                    , testGroup "lessThan20" $
                        map (\(n, t) -> testCase (show n ++ " is " ++ t) $ lessThan20 n @?= t)
                            (zip [1..] lessThan20Answers)
                    , testGroup "tens" $
                        map (\(n, t) -> testCase (show (n * 10) ++ " is " ++ t) $ tens n @?= t)
                            (zip [2..] tensAnswers)
                    , testGroup "number"
                        [ testCase "1 is one" $ number 1 @?= "one"
                        , testCase "5 is five" $ number 5 @?= "five"
                        , testCase "10 is ten" $ number 10 @?= "ten"
                        , testCase "11 is eleven" $ number 11 @?= "eleven"
                        , testCase "19 is nineteen" $ number 19 @?= "nineteen"
                        , testCase "20 is twenty" $ number 20 @?= "twenty"
                        , testCase "25 is twenty five" $ number 25 @?= "twenty five"
                        , testCase "50 is fifty" $ number 50 @?= "fifty"
                        , testCase "59 is fifty nine" $ number 59 @?= "fifty nine"
                        , testCase "90 is ninety" $ number 90 @?= "ninety"
                        , testCase "91 is ninety one" $ number 91 @?= "ninety one"
                        , testCase "99 is ninety nine" $ number 99 @?= "ninety nine"
                        , testCase "100 is one hundred" $ number 100 @?= "one hundred"
                        ]
                    ]

main :: IO ()
main = defaultMain fizzBuzzSuite
