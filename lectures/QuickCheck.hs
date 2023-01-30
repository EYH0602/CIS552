{-# LANGUAGE ScopedTypeVariables #-}

module QuickCheck where

import Control.Monad (liftM2, liftM3)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    OrderedList (..),
    Property,
    Testable (..),
    choose,
    classify,
    elements,
    forAll,
    frequency,
    label,
    oneof,
    quickCheck,
    sample,
    sized,
    withMaxSuccess,
    (==>),
  )

quickCheckN :: (Testable prop) => Int -> prop -> IO ()
quickCheckN n = quickCheck . withMaxSuccess n

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys

prop_revapp_ok :: [Int] -> [Int] -> Bool
prop_revapp_ok xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

-- QuickCheck QuickSort

-- this qsort has a bug intentionally
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = [y | y <- xs, y < x]
    rhs = [z | z <- xs, z > x]

isOrdered :: Ord a => [a] -> Bool
isOrdered (x : y : zs) = x <= y && isOrdered (y : zs)
isOrdered [_] = True
isOrdered [] = True

prop_qsort_isOrdered :: [Int] -> Bool
prop_qsort_isOrdered xs = isOrdered (qsort xs)

prop_qsort_idemp :: [Int] -> Bool
prop_qsort_idemp xs = qsort (qsort xs) == qsort xs

prop_qsort_min :: [Int] -> Bool
prop_qsort_min xs = head (qsort xs) == minimum xs

prop_qsort_nn_min :: [Int] -> Property
prop_qsort_nn_min xs =
  not (null xs) ==> head (qsort xs) == minimum xs

prop_qsort_nn_max :: [Int] -> Property
prop_qsort_nn_max xs =
  not (null xs) ==> last (qsort xs) == maximum xs

prop_qsort_sort :: [Int] -> Bool
prop_qsort_sort xs = qsort xs == List.sort xs

isDistinct :: Eq a => [a] -> Bool
isDistinct [] = True
isDistinct (x : xs) = x `notElem` xs && isDistinct xs

prop_qsort_distinct :: [Int] -> Bool
prop_qsort_distinct = isDistinct . qsort

prop_qsort_distinct_sort :: [Int] -> Property
prop_qsort_distinct_sort xs =
  isDistinct xs ==> qsort xs == List.sort xs

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort = foldr List.insert []

prop_isort_sort :: [Int] -> Bool
prop_isort_sort xs = isort xs == List.sort xs

prop_insert_ordered' :: Int -> [Int] -> Bool
prop_insert_ordered' x xs = isOrdered (insert x xs)

prop_insert_ordered :: Int -> [Int] -> Property
prop_insert_ordered x xs =
  isOrdered xs ==> isOrdered (insert x xs)

prop_insert_ordered_vacuous :: Int -> [Int] -> Bool
prop_insert_ordered_vacuous x xs =
  not (isOrdered xs) || isOrdered (insert x xs)

prop_insert_ordered_vacuous' :: Int -> [Int] -> Property
prop_insert_ordered_vacuous' x xs =
  label lbl $
    not (isOrdered xs) || isOrdered (insert x xs)
  where
    lbl =
      (if isOrdered xs then "Ordered, " else "Not Ordered, ")
        ++ show (length xs)

-- Generator Practice

genThree :: Gen Int -- a generator that always generates the value '3'
genThree = return 3

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair = liftM2 (,) -- a generator for pairs

genBool :: Gen Bool
genBool = elements [True, False]

genTriple :: Gen a -> Gen b -> Gen c -> Gen (a, b, c)
genTriple = liftM3 (,,)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe ga = oneof [return Nothing, fmap Just ga]

genList1 :: (Arbitrary a) => Gen [a]
genList1 = liftM2 (:) arbitrary genList1

genList2 :: (Arbitrary a) => Gen [a]
genList2 =
  oneof
    [ return [],
      liftM2 (:) arbitrary genList2
    ]

genList3 :: (Arbitrary a) => Gen [a]
genList3 =
  frequency
    [ (1, return []),
      (7, liftM2 (:) arbitrary genList3)
    ]

genList4 :: forall a. (Arbitrary a) => Gen [a]
genList4 = sized gen
  where
    gen :: Int -> Gen [a]
    gen n =
      frequency
        [ (1, return []),
          (n, liftM2 (:) arbitrary (gen (n `div` 2)))
        ]

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized gen
    where
      gen n =
        frequency
          [ (1, return Empty),
            (n, liftM3 Branch arbitrary (gen (n `div` 2)) (gen (n `div` 2)))
          ]

genOrdList :: (Arbitrary a, Ord a) => Gen [a]
genOrdList = fmap List.sort genList3

prop_insert :: Int -> Property
prop_insert x = forAll genOrdList $ \xs ->
  isOrdered xs && isOrdered (insert x xs)

-- smarter test-case generation
newtype OrdList a = OrdList [a] deriving (Eq, Ord, Show, Read)

instance (Ord a, Arbitrary a) => Arbitrary (OrdList a) where
  arbitrary = fmap OrdList genOrdList

prop_insert' :: Int -> OrdList Int -> Bool
prop_insert' x (OrdList xs) = isOrdered $ insert x xs

prop_insert'' :: Int -> OrderedList Int -> Bool
prop_insert'' x (Ordered xs) = isOrdered $ insert x xs
