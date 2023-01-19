{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Merge Sort
==========
-}

module MergeSort where

import Data.Foldable (fold)
import qualified Data.List as List
import Data.Monoid
import SortedList (SortedList)
import qualified SortedList as SL
import Test.HUnit

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x : xs)
  | x >= n = n : x : xs
  | otherwise = x : insert n xs

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

-------------------------------------------------------------

{- SortedLists to the Rescue -}

sortedFromList :: Ord a => [a] -> SortedList a
sortedFromList = SL.fromList . List.sort

sortedListSort :: Ord a => [a] -> [a]
sortedListSort = SL.toList . sortedFromList

testSortedFromList :: Test
testSortedFromList =
  let unsorted = [51, 67, 89, 95, 14, 31, 28, 87, 0, 25]
      sorted = [0, 14, 25, 28, 31, 51, 67, 87, 89, 95]
   in sortedListSort unsorted ~?= sorted

sortedFromList' :: Ord a => [a] -> SortedList a
sortedFromList' = foldMapList SL.singleton

sortedListSort' :: Ord a => [a] -> [a]
sortedListSort' = SL.toList . sortedFromList'

testSortedFromList' :: Test
testSortedFromList' =
  let unsorted :: [Int]
      unsorted = [47, 80, 28, 47, 45, 76, 1, 35, 19, 1]
   in sortedListSort' unsorted ~?= sortedListSort unsorted -- old & new agree

foldMapList :: Monoid m => (a -> m) -> [a] -> m
foldMapList f = foldr ((<>) . f) mempty

sumOfProducts :: Num a => [[a]] -> a
sumOfProducts = sum' . map product'
  where
    product' :: Num a => [a] -> a
    product' = getProduct . foldMap Product
    sum' :: Num a => [a] -> a
    sum' = getSum . foldMap Sum

testSumOfProducts :: Test
testSumOfProducts = sumOfProducts [[1], [2, 3], [4, 5, 6], [7, 8, 9, 10]] ~?= (5167 :: Int)

benchmark :: IO ()
benchmark = (print . last . sortedListSort') ([10000, 9999 .. 0] :: [Int])

{-
(Try it yourself by setting  `:set +s` in ghci. On my machine, it take 26.61 secs
and allocates 17,604,539,672 bytes))

For any singleton `SortedList [a]` and any other `SortedList as`, computing
`SortedList [a] <> SortedList as` is identical not only in resultant value,
but also in algorithmic structure to computing the result of `insert a
as`. The definition of `foldMapList` linearly scans across its input list,
successively combining values using `(<>)` -- and so, like insertion sort, the
whole whole algorithm ends up executing a quadratic number of comparisons.

A real merge sort algorithm, as you likely know, divides its input more
intelligently than the one we've written above in terms of `foldMapList`. By
dividing its input roughly in half every iteration, it only has to do a
logarithmic amount of merging.

To make our merge sort do this, we need to use a different kind of `foldMap`!

The Foldable Typeclass
----------------------

At this point, I'd like to point something out: `foldMapList` can itself be
even further generalized. We already know that lists are not the only data
structures which support folding -- we've seen folds for trees of various kinds
and for other data structures as well. As a result, it makes sense to allow
some kind of `foldMap` operation for those structures also. In the standard
library, we therefore have:

< foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

That is to say, `foldMap` is a method of yet another type class, `Foldable`, of
which the type constructor `[]` is an instance. Implementing this interface
roughly corresponds to saying, "this data structure contains some elements, and
I know how to do a fold across them." To implement the `Foldable` class for some
type, we just need to implement `foldMap`.

Implement the `Functor` and `Foldable` instances for the `Crispy` datatype.
Remember to keep in mind a guiding principle: when you are confused, don't think
about what things are supposed to mean; just follow the types and see where they
take you.
-}

data Crispy a
  = Snap a [a] a
  | Crackle [[Crispy a]]
  | Pop Integer
  deriving (Eq, Show)

instance Functor Crispy where
  fmap = undefined

instance Foldable Crispy where
  foldMap = undefined

testCrispy :: Test
testCrispy =
  let c1, c2, c3, c5 :: Crispy Integer
      c1 = fmap (+ 1) (Snap 0 [1, 2, 3] 4)
      c2 = Snap 700 [] 600
      c3 = Pop 1234567890
      c5 = fmap (subtract 1) (Crackle [[c1, c2], [c1, c3]])
   in TestList
        [ 15 ~?= getSum (foldMap Sum c1),
          1 ~?= getProduct (foldMap Product c3),
          "0123469959901234" ~?= foldMap show c5
        ]

-------------------------------------------------------------

{-
Back to Sorting
---------------

In order to express an efficient merge sort in terms of `foldMap`, we need to
design a data structure that represents a sequence of elements (just like a
list), but whose `Foldable` instance uses a divide-and-conquer strategy, rather
than the `[]` instance's linear fold pattern of recursion.
-}

newtype DivideList a = DivideList {getDivideList :: [a]} deriving (Eq, Show)

{-
That means that we need `DivideList`s to be `Foldable`, but in a
different way. First, implement the `divide` function, which splits a
`DivideList` in its middle, returning the result of the split.
-}

divide :: DivideList a -> (DivideList a, DivideList a)
divide = undefined

testDivide :: Test
testDivide =
  TestList
    [ divide (DivideList "abcd")
        ~?= (DivideList "ab", DivideList "cd"),
      divide (DivideList "abcde")
        ~?= (DivideList "ab", DivideList "cde"),
      divide (DivideList "")
        ~?= (DivideList "", DivideList "")
    ]

{-
Using this function, we can define the `Foldable` instance for `DivideList`s.
Note that this definition is trickier than it seems. If you encounter an
infinite loop, it means that you have not covered one of a particular set of
slightly non-trivial edge cases.
-}

instance Foldable DivideList where
  foldMap f xs =
    case divide xs of
      (DivideList as, DivideList bs) -> undefined

testDivideList :: Test
testDivideList =
  let xs = DivideList [1, 2, 3]
      ys = DivideList []
   in TestList
        [ Product (6 :: Int) ~?= foldMap Product xs,
          Sum (0 :: Int) ~?= foldMap Sum ys
        ]

{-
Now that we know how general the `foldMap` function is, have a look at the
implementation of `sortedListSort'` above -- does its input type need to only be
a list? Generalize its type signature so that it outputs a list of sorted
elements located inside an arbitrary `Foldable` structure.
-}

-- foldSort ::
foldSort = undefined -- implementation should use foldMap

{-
By parameterizing over any `Foldable` container, what we've done is to *factor
out the folding strategy* into the choice of original container! To pick a
different divide-and-conquer strategy, we need only specify a different
container type, and give it a `Foldable` instance that folds along different
creases.

So, while our `sortedListSort` was O(N ^ 2), we can produce a differently
structured algorithm by instead folding over a `DivideList` instead:
-}

realMergeSort :: Ord a => [a] -> [a]
realMergeSort = foldSort . DivideList

{-
If you've done everything correctly, this main function should return rather
quickly. This is much faster than the example above. On my machine it takes
(0.07 secs, 41,595,632 bytes).
-}

main :: IO ()
main = (print . last . realMergeSort) ([10000, 9999 .. 0] :: [Int])

{-
Concluding Thoughts About This Exercise
---------------------------------------

The important takeaway here is this: `foldMap` defines once and for all a
universal "divide-and-conquer" algorithm -- all we need to do to use it is to
provide a way to "divide" an input container (i.e. give a `Foldable instance`),
then give a way to compute on those elements (i.e. the mapped function `a -> m`)
and a way to combine ("conquer") the results of that computation (i.e. a
`Monoid` instance for the result type).

Almost any divide-and-conquer algorithm can be fit into this framework, and that
means we can avoid repeating ourselves when writing such programs. We can reuse
the division strategy of `DivideList` when writing some other algorithm, and
likewise for the sorted-merging combination strategy of `SortedList`.
-}
