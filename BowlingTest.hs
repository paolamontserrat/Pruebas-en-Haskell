module Main where
-- Paola Montserrat Ruiz Carmen y America Citlalli Lopez Lemus
import Test.Tasty
import Test.Tasty.HUnit

import Bowling

tests :: [(String, [Int], Maybe [Frame])]
tests =
  [ ("zeros are open 0 0"
    , replicate 20 0
    , Just $ replicate 10 (Open 0 0)
    )
  , ("ones are open 1 1"
    , replicate 20 1
    , Just $ replicate 10 (Open 1 1)
    )
  , ("4+5s are open 4 5"
    , take 20 $ cycle [4, 5]
    , Just $ replicate 10 (Open 4 5)
    )
  , ("spares in non last position"
    , let spare = [1, 9]
          opens n = take (2*n) $ cycle [3, 3]
      in spare ++ opens 2 ++ spare ++ opens 4 ++ spare ++ opens 1
    , let spare = [Spare 1 3]
          opens n = replicate n (Open 3 3)
      in Just $ spare ++ opens 2 ++ spare ++ opens 4 ++ spare ++ opens 1
    )
  , ("spare in last position"
    , take 18 (cycle [3, 3]) ++ [1, 9, 5]
    , Just $ replicate 9 (Open 3 3) ++ [Spare 1 5]
    )
  , ("strike in non last position"
    , let strike = [10]
          opens n = take (2*n) $ cycle [3, 3]
      in strike ++ opens 2 ++ strike ++ opens 4 ++ strike ++ opens 1
    , let strike = [Strike 3 3]
          opens n = replicate n (Open 3 3)
      in Just $ strike ++ opens 2 ++ strike ++ opens 4 ++ strike ++ opens 1
    )
  , ("strike in last position"
    , take 18 (cycle [3, 3]) ++ [10, 5, 5]
    , Just $ replicate 9 (Open 3 3) ++ [Strike 5 5]
    )
  , ("ill formed play"
    , [0, 1]
    , Nothing
    )
  , ("invalid negative value"
    , [1, -1, 2, 3]
    , Nothing
    )
  , ("too many rolls"
    , replicate 22 1
    , Nothing
    )
  , ("too few rolls"
    , replicate 19 1
    , Nothing
    )
  , ("invalid frame sum"
    , [5, 6, 3, 4]
    , Nothing
    )
  , ("invalid frame sum in last frame"
    , take 18 (cycle [3, 3]) ++ [5, 6, 5]
    , Nothing
    )
  , ("invalid strike in last frame"
    , take 18 (cycle [3, 3]) ++ [10, 5, 6]
    , Just $ replicate 9 (Open 3 3) ++ [Strike 5 6]
    )
  , ("invalid spare in last frame"
    , take 18 (cycle [3, 3]) ++ [5, 5, 6]
    , Just $ replicate 9 (Open 3 3) ++ [Spare 5 6]
    )
  ]

bowlingSuite :: TestTree
bowlingSuite = testGroup "Bowling tests"
  [ testGroup "toFrames" $
    map (\(label, input, expected) ->
      testCase label $ toFrames input @?= expected) tests
  , testGroup "score"
    [ let Just frames = toFrames [1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6]
      in testCase "spec example" $ score frames @?= 133
    , let Just frames = toFrames (replicate 12 10)
      in testCase "all strikes" $ score frames @?= 300
    , let Just frames = toFrames (take 21 $ cycle [5, 5])
      in testCase "all spares" $ score frames @?= 150
    
    ]
  ]

main :: IO ()
main = defaultMain bowlingSuite
