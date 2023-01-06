module Datatypes where

import Test.HUnit ()
import Prelude hiding (Either, Just, Left, Maybe, Nothing, Right)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq)

nextWeekday :: Day -> Day
nextWeekday Monday = Tuesday
nextWeekday Tuesday = Wednesday
nextWeekday Wednesday = Thursday
nextWeekday Thursday = Friday
nextWeekday Friday = Monday
nextWeekday Saturday = Monday
nextWeekday Sunday = Monday

twoBusinessDays :: Day -> Day
twoBusinessDays = nextWeekday . nextWeekday

data Shape
  = Circle Double Double Double
  | Rectangle Double Double Double Double
  deriving (Eq, Show)

area :: Shape -> Double
area (Circle x y r) = pi * r * r
area (Rectangle llx lly urx ury) = width * height
  where
    width = urx - llx
    height = ury - lly

-- Records

data Point = Point {x :: Double, y :: Double}
  deriving (Show, Eq)

point1 :: Point
point1 = Point {y = 1.0, x = 2.0} -- order doesn't matter

-- Be careful, Haskell will let you leave the field names off
-- but here the order does matter
point2 :: Point
point2 = Point 1.0 1.0

-- point3 is a Point with x component equal to 2.0,
-- and all others (which is only y here) the same as point1
point3 :: Point
point3 = point1 {x = 2.0}

distFromOrigin :: Point -> Double
distFromOrigin Point {x = px, y = py} = sqrt (px * px + py * py)

distFromOrigin' :: Point -> Double
distFromOrigin' p = sqrt (px ^ 2 + py ^ 2)
  where
    px = x p
    py = y p

-- Recursive Datatypes
data IntListNE
  = ISingle Int
  | ICons Int IntListNE

safeHead :: IntListNE -> Int
safeHead (ISingle x) = x
safeHead (ICons x _) = x

oneTwoThree :: IntListNE
oneTwoThree = ICons 1 (ICons 2 (ISingle 3))

sumOfIntListNE :: IntListNE -> Int
sumOfIntListNE (ISingle x) = x
sumOfIntListNE (ICons x xs) = x + sumOfIntListNE xs

-- Polymorphic Datatypes
data Maybe a = Just a | Nothing deriving (Eq, Show)

noInt :: Maybe Int
noInt = Nothing

justTrue :: Maybe Bool
justTrue = Just True

justThree :: Maybe Int
justThree = Just 3

data Either a b = Left a | Right b

safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "You can't divide by zero, silly."
safeDiv x y = Right $ x `div` y

-- Trees
data Tree a
  = Empty
  | Branch a (Tree a) (Tree a)
  deriving (Eq, Show)

exTree :: Tree Int
exTree =
  Branch
    5
    (Branch 2 (Branch 1 Empty Empty) (Branch 4 Empty Empty))
    (Branch 9 Empty (Branch 7 Empty Empty))

treePlus :: Tree Int -> Int -> Tree Int
treePlus Empty _ = Empty
treePlus (Branch x l r) inc = Branch (x + inc) (treePlus l inc) (treePlus r inc)

infixOrder :: Tree a -> [a]
infixOrder Empty = []
infixOrder (Branch x l r) = infixOrder l ++ [x] ++ infixOrder r

prefixOrder :: Tree a -> [a]
prefixOrder Empty = []
prefixOrder (Branch x l r) = [x] ++ prefixOrder l ++ prefixOrder r

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Empty = Empty
treeMap f (Branch x l r) = Branch (f x) (treeMap f l) (treeMap f r)

treeIncr :: Tree Int -> Tree Int
treeIncr = treeMap (+1)

main :: IO ()
main = do
  print (twoBusinessDays Sunday)
  print point2
  print (x point1)
  print (distFromOrigin point1)
  print (distFromOrigin' point1)
  print (safeHead oneTwoThree)
  print (sumOfIntListNE oneTwoThree)
  print exTree
  print (treePlus exTree 1)
  print (prefixOrder exTree)
