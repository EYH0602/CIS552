{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SortedList
  ( SortedList, -- the abstract type (and its instances)
    singleton, -- other functions
    toList,
    fromList,
    minimum,
    numDistinct,
    count,
    foldr,
    length,
    filter,
    ten,
    twentyfour,
    whyNoFunctor,
    testListMonoid,
    testSortedList,
    testMinimum,
    testNumDistinct,
    testCount,
  )
where

import qualified Data.List as List
-- you can use List library functions, prefixed with `List.`

import Data.Semigroup -- also re-exports much of Data.Monoid
import Test.HUnit (Test (TestList), runTestTT, (~?=))
import Prelude hiding (filter, foldr, length, minimum)

testListMonoid :: Test
testListMonoid =
  let t1, t2, t3 :: [Int]
      t1 = [1, 2]
      t2 = [3, 4]
      t3 = [1, 2, 3, 4]
   in TestList
        [ mempty <> t1 ~?= t1, -- left identity
          t1 <> mempty ~?= t1, -- right identity
          (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
        ]

foldList :: Monoid b => [b] -> b
foldList = List.foldr (<>) mempty

ten :: Int
ten = getSum (foldList (map Sum [1, 2, 3, 4]))

twentyfour :: Int
twentyfour = getProduct (foldList (map Product [1, 2, 3, 4]))

---------------------------------------------------------

newtype SortedList a = SL [a] deriving (Eq, Show)

{-
We can use pattern matching to convert the sorted list into a regular list.
-}

-- | convert to a regular list. The elements should be produced in order.
toList :: SortedList a -> [a]
toList (SL as) = as

-- | convert from a regular list.
fromList :: Ord a => [a] -> SortedList a
fromList = foldList . map singleton

{-
Some of the operations that we define for sorted lists just delegate
to the version for regular lists.
-}

-- | construct a sorted list containing a single element
singleton :: a -> SortedList a
singleton a = SL [a]

-- | reduce a SortedList in order
foldr :: (a -> b -> b) -> b -> SortedList a -> b
foldr f b (SL xs) = List.foldr f b xs

-- | decide which elements of the sorted list to keep
filter :: (a -> Bool) -> SortedList a -> SortedList a
filter f (SL xs) = SL (List.filter f xs)

-- | count the number of elements in the sorted list
length :: SortedList a -> Int
length (SL xs) = List.length xs

{-
However, the `Monoid` instance can take advantage of the
sortedness of this data structure.

Now, fill in the `Monoid` instance for `SortedList`s. You should ensure that the
list is always sorted with smaller elements (according to `(<=)` coming
before larger elements.)

Hint: keep in mind the properties of sorted lists when writing this
instance. This invariant lets you write faster code than you would otherwise
be able to do.
-}

instance Ord a => Semigroup (SortedList a) where
  l1 <> l2 = SL (merge ll1 ll2)
    where
      ll1 = toList l1
      ll2 = toList l2
      merge xs ys = List.sort $ xs ++ ys

instance Ord a => Monoid (SortedList a) where
  mempty = SL []

{-
Make sure that your implementation only produces sorted lists, and also
satisfies the properties of monoids!
-}

testSortedList :: Test
testSortedList =
  let t1, t2, t3 :: SortedList Int
      t1 = SL [2, 4]
      t2 = SL [1, 5]
      t3 = SL [2, 3]
   in TestList
        [ t1 <> t3 ~?= SL [2, 2, 3, 4], -- <> preserves sorting
          mempty <> t1 ~?= t1, -- left identity
          t1 <> mempty ~?= t1, -- right identity
          (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
        ]

---------------------------------------------------------

{-
Invariant-sensitive Operations
------------------------------

Note that we didn't *have* to define `foldr`, `filter`, and `length` for `SortedList`s.
The clients of the module could have also defined these operations themselves by using
`toNormalList` and the `Monoid` operations.

However, by defining operations in this module, we
While merely the operations defined above are sufficient to define the analogues
of most list functions for `SortedList`s also, implementing a replica of the
list library only in terms of the above abstraction would necessarily come at a
performance cost; it would necessitate conversion to and from the `SortedList`
representation, which requires computational work.

On the other hand, if we were to implement these functions *here*, we could
take advantage of the internal sorted-ness invariant of the list in order to
make certain operations *faster*. Let's do that.

A first example: `minimum`. (Note: this definition does not have the same type
as the `minimum` function in the standard library.)
-}

minimum :: SortedList a -> Maybe a
minimum = undefined

testMinimum :: Test
testMinimum =
  let t1, t2, t3 :: SortedList Int
      t1 = SL [1, 3, 5]
      t2 = SL []
      t3 = SL [1, error "kaboom!"] <> SL [2]
   in TestList
        [ minimum t1 ~?= Just 1, -- the minimum of a non-empty sorted list
          minimum t2 ~?= Nothing, -- the minimum of an empty sorted list
          minimum t3 ~?= Just 1 -- minimum need not examine whole list
        ]

{-
In the above test cases, you will get an error if your implementation does not
take advantage of the sorted-ness invariant to avoid extra computation.

Another operation which can be made more efficient for `SortedList`s is
calculating the number of distinct values in the list.
-}

numDistinct :: Ord a => SortedList a -> Int
numDistinct = undefined

testNumDistinct :: Test
testNumDistinct =
  TestList
    [ numDistinct (SL [1 :: Int, 1, 3, 3, 5]) ~?= 3,
      numDistinct (SL ([] :: [Int])) ~?= 0
    ]

{-
We can also count how many times every distinct value occurs in the list:
-}

count :: Eq a => SortedList a -> SortedList (a, Integer)
count = undefined

{-
Your implementation of `count` should result in another genuine, legal
`SortedList`. Convince yourself that it does before moving on, keeping in mind
the `Ord` instances for tuples are left-to-right lexicographic orderings,
dependent on the underlying `Ord` instances of the tuple's elements.
-}

testCount :: Test
testCount =
  let xs = SL "abbcccdddd"
   in count xs ~?= SL [('a', 1), ('b', 2), ('c', 3), ('d', 4)]

{-
At this point, one important typeclass seems to have been left out in our
interface to the `SortedList` type: `Functor`. It seems natural that we should
be able to map a function over a `SortedList`, just like we can over an ordinary
list. This doesn't work, though. Why?
-}

whyNoFunctor :: String
whyNoFunctor = undefined

{-
At this point, we have finished defining the internal implementation of
`SortedList`s. Because all the operations we expose to the user of this module
respect the sorted-ness property of `SortedList`s, we know that any value of
this type must be sorted. So, once we go back to the file `Main.lhs`, we will be
we are prevented from making "illegal" values of `SortedList`s.

-}
main :: IO ()
main = do
  _ <- runTestTT $ TestList [testSortedList]
  return ()
