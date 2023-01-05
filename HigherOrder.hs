module HigherOrder where

import Data.Char
import Test.HUnit
import Prelude hiding (filter, foldl, foldr, map, pred, product, sum)

plus1 :: Int -> Int
plus1 = (+) 1

minus1 :: Int -> Int
minus1 = (-) 1

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

ex1 :: (a -> a) -> a -> a
ex1 = doTwice doTwice

-- use foldr to implement things

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _f base [] = base
foldr f base (x : xs) = x `f` foldr f base xs

sum :: [Int] -> Int
sum = foldr (+) 0

product :: [Int] -> Int
product = foldr (*) 1

len :: [a] -> Int
len = foldr (\_ x -> x + 1) 0

factorial :: Int -> Int
factorial n = product [1 .. n]

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr f' []
  where
    f' x acc = if f x then x : acc else acc

map :: (a -> b) -> [a] -> [b]
map f = foldr f' []
  where
    f' x acc = f x : acc

-- not from class
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f = foldr (flip f)

main :: IO ()
main = do
  print (ex1 (+ 10) 5)
  print (len "mmm donuts!" :: Int)
  print (factorial 20)
  print (filter even [1 .. 10])
  print (map (+ 1) [1 .. 5])
  print (foldl (+) 0 [1 .. 5])

dtTests :: Test
dtTests =
  TestList
    [ doTwice plus1 4 ~?= 6,
      doTwice minus1 5 ~?= 3
    ]

ex1Test :: Test
ex1Test = ex1 (+ 10) 5 ~?= 45

testFilter :: Test
testFilter =
  TestList
    [ filter (> 10) [1 .. 20] ~?= ([11 .. 20] :: [Int]),
      filter (\l -> sum l <= 42) [[10, 20], [50, 50], [1 .. 5]] ~?= [[10, 20], [1 .. 5]]
    ]
