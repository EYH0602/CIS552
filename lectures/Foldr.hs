module Foldr where

import Test.HUnit (Counts, Test (TestList), runTestTT, (~:), (~=?), (~?=))
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

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x : acc else acc) []

reverse :: [a] -> [a]
reverse = foldr (\x acc -> acc ++ [x]) []

intersperse :: a -> [a] -> [a]
intersperse a = foldr f []
  where
    f x [] = [x]
    f x acc = x : a : acc

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z xs = foldr (flip f) z (reverse xs)

main :: IO ()
main = do
  print "a"
  print (all (> 10) [1 .. 20])
  print (last "abcd")
  print (last "")
  print (filter (> 2) [2, 3])
  print (reverse "cba")
  print (intersperse ',' "abc")
  print (foldl (++) "x" ["1", "2", "3"])
  runTests

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

testFilter :: Test
testFilter =
  "filter"
    ~: TestList
      [ filter (> 10) [1 .. 20] ~?= ([11 .. 20] :: [Int]),
        filter (\l -> sum l <= 42) [[10, 20], [50, 50], [1 .. 5]] ~?= ([[10, 20], [1 .. 5]] :: [[Int]])
      ]

testReverse :: Test
testReverse =
  "reverse"
    ~: TestList
      [ reverse "abcd" ~?= "dcba",
        reverse "" ~?= ""
      ]

testIntersperse :: Test
testIntersperse =
  "intersperse"
    ~: TestList
      [ "intersperse0" ~: intersperse ',' "abcde" ~=? "a,b,c,d,e",
        "intersperse1" ~: intersperse ',' "" ~=? ""
      ]

testFoldl :: Test
testFoldl = foldl (++) "x" ["1", "2", "3"] ~=? "x123"

runTests :: IO ()
runTests = do
  _ <-
    runTestTT $
      TestList
        [ testLength,
          testAll,
          testLast,
          testFilter,
          testReverse,
          testIntersperse,
          testFoldl
        ]
  return ()
