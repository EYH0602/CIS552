{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuickList where

import Data.List as List
import Data.List.NonEmpty (nonEmpty)
import Test.QuickCheck

prop_const :: Eq a => (a -> a -> a) -> a -> a -> Bool
prop_const const' a b = const' a b == a

constBug :: a -> a -> a
constBug _ b = b -- Oops: this returns the *second* argument, not the first.

data Undefined

instance Testable Undefined where
  property = error "Unimplemented property"

prop_minimum :: Ord a => ([a] -> a) -> [a] -> Property
prop_minimum minimum' xs = not (null xs) ==> minimum' xs == minimum xs

minimumBug :: Ord a => [a] -> a
minimumBug = maximum

newtype SmallNonNegInt = SmallNonNegInt Int deriving (Eq, Ord, Show, Read)

instance Arbitrary SmallNonNegInt where
  arbitrary :: Gen SmallNonNegInt
  arbitrary = fmap SmallNonNegInt (elements [1 .. 100])

  shrink :: SmallNonNegInt -> [SmallNonNegInt]
  shrink (SmallNonNegInt x) = fmap SmallNonNegInt (shrink x)

prop_replicate :: Eq a => (Int -> a -> [a]) -> SmallNonNegInt -> a -> Bool
prop_replicate replicate' (SmallNonNegInt n) x = replicate' n x == replicate n x

replicateBug :: Int -> a -> [a]
replicateBug n = replicate (n -1)

prop_group_1 :: Eq a => ([a] -> [[a]]) -> [a] -> Bool
prop_group_1 group' xs = concat (group' xs) == xs

prop_group_2 :: Eq a => ([a] -> [[a]]) -> [a] -> Bool
prop_group_2 group' = all (\xs -> not (null xs) && allSame xs) . group'
  where
    allSame (x : xs) = all (== x) xs

groupBug :: Eq a => [a] -> [[a]]
groupBug = map ((: []) . head) . group

prop_reverse_1 :: ([a] -> [a]) -> [a] -> Bool
prop_reverse_1 reverse' xs = length (reverse' xs) == length xs

-- Reflexive
prop_reverse_2 :: Eq a => ([a] -> [a]) -> [a] -> Bool
prop_reverse_2 reverse' xs = xs == (reverse' . reverse') xs

reverseBug_1 :: [a] -> [a]
reverseBug_1 = tail . reverse

reverseBug_2 :: [a] -> [a]
reverseBug_2 xs = head rev : rev
  where
    rev = tail (reverse xs)

main :: IO ()
main = do
  let qcName name prop = do
        putStr $ name ++ ": "
        quickCheck prop

  putStrLn "The following tests should all succeed:"
  qcName "const" $ prop_const (const :: Char -> Char -> Char)
  qcName "minimum" $ prop_minimum (minimum :: String -> Char)
  qcName "replicate" $ prop_replicate (replicate :: Int -> Char -> String)
  qcName "group_1" $ prop_group_1 (group :: String -> [String])
  qcName "group_2" $ prop_group_2 (group :: String -> [String])
  qcName "reverse_1" $ prop_reverse_1 (reverse :: String -> String)
  qcName "reverse_2" $ prop_reverse_2 (reverse :: String -> String)

  putStrLn ""

  putStrLn "The following tests should all fail:"
  qcName "const" $ prop_const (constBug :: Char -> Char -> Char)
  qcName "minimum" $ prop_minimum (minimumBug :: String -> Char)
  qcName "replicate" $ prop_replicate (replicateBug :: Int -> Char -> String)
  qcName "group_1" $ prop_group_1 (groupBug :: String -> [String])
  qcName "group_2" $ prop_group_2 (groupBug :: String -> [String])
  qcName "reverse_1" $ prop_reverse_1 (reverseBug_1 :: String -> String)
  qcName "reverse_2" $ prop_reverse_2 (reverseBug_2 :: String -> String)
