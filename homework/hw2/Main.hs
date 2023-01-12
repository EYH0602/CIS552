module Main where

import Play
import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))
import XMLTypes
import Prelude hiding (all, concat, takeWhile)

-- | `takeWhile`, applied to a predicate `p` and a list `xs`,
-- returns the longest prefix (possibly empty) of `xs` of elements
-- that satisfy `p`.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x acc -> if p x then x : acc else []) []

ttakeWhile :: Test
ttakeWhile =
  "takeWhile"
    ~: TestList
      [ takeWhile (< 3) [1, 2, 3, 4, 1, 2, 3, 4] ~?= [1, 2],
        takeWhile (< 9) [1, 2, 3] ~?= [1, 2, 3],
        takeWhile (< 0) [1, 2, 3] ~?= []
      ]

-- | `find pred lst` returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a `Maybe`.
find :: (a -> Bool) -> [a] -> Maybe a
find p = foldr (\x acc -> if p x then Just x else acc) Nothing

tfind :: Test
tfind =
  "find"
    ~: TestList
      [ find odd [0, 2, 3, 4] ~?= Just 3,
        find odd [1, 2, 3, 4] ~?= Just 1,
        find even [1, 3, 5] ~?= Nothing
      ]

-- | `all pred lst` returns `False` if any element of `lst`
-- fails to satisfy `pred` and `True` otherwise.
all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x acc -> p x && acc) True

tall :: Test
tall = "all" ~: TestList [all odd [1 .. 3] ~?= False]

-- | `map2 f xs ys` returns the list obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- >>> map2 (+) [1,2] [3,4]
-- [4,6]
--
-- NOTE: `map2` is called `zipWith` in the Prelude
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f xs ys = foldr (\(a, b) acc -> f a b : acc) [] (zip xs ys)

tmap2 :: Test
tmap2 = "map2" ~: TestList [map2 (+) [1, 2] [3, 4] ~?= [4, 6]]

-- | Apply a partial function to all the elements of the list,
-- keeping only valid outputs.
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr f' []
  where
    f' x acc = case f x of
      Nothing -> acc
      Just res -> res : acc

tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~: TestList [mapMaybe root [0.0, -1.0, 4.0] ~?= [0.0, 2.0]]

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

----------------------------------------------------------------------

main :: IO ()
main = doTests

doTests :: IO ()
doTests = do
  _ <-
    runTestTT $
      TestList
        [ testHO
        --   testFoldr,
        --   testTree,
        --   testXML
        ]
  return ()

testHO :: Test
testHO =
  TestList
    [ ttakeWhile,
      tfind,
      tall,
      tmap2,
      tmapMaybe
    ]
