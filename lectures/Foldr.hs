module Foldr where

import Test.HUnit (Counts, Test (TestList), runTestTT, (~:), (~?=))
import Prelude hiding (all, filter, foldl, foldl1, last, length, map, reverse)

length :: [a] -> Int
length = foldr (\_ n -> 1 + n) 0

all :: (a -> Bool) -> [a] -> Bool
all f = foldr (\x y -> f x && y) True

last :: [a] -> Maybe a
last = foldr f Nothing
  where
    f x Nothing = Just x
    f _ acc = acc

main :: IO ()
main = do
  print "a"
  print (all (> 10) [1 .. 20])
  print (last "abcd")
  print (last "")

testLength :: Test
testLength =
  "length"
    ~: TestList
      [ length "abcd" ~?= 4,
        length "" ~?= 0
      ]

testAll :: Test
testAll =
  "all"
    ~: TestList
      [ all (> 10) ([1 .. 20] :: [Int]) ~?= False,
        all (> 0) ([1 .. 20] :: [Int]) ~?= True
      ]

testLast :: Test
testLast =
  "last"
    ~: TestList
      [ last "abcd" ~?= Just 'd',
        last "" ~?= Nothing
      ]
