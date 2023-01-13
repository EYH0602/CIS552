module Main where

import Play
import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))
import XMLTypes
import Prelude hiding (all, concat, takeWhile, zip, (++))

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
-- is (:) considered list function?
(++) :: [a] -> [a] -> [a]
(++) = flip $ foldr (:)

-- | The concatenation of all of the elements of a list of lists
concat :: [[a]] -> [a]
concat = foldr (++) []

tconcat :: Test
tconcat = "concat" ~: TestList [concat [[1, 2, 3], [4, 5, 6], [7, 8, 9]] ~?= [1, 2, 3, 4, 5, 6, 7, 8, 9]]

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
startsWith :: String -> String -> Bool
startsWith xs ys = foldr (\(x, y) acc -> x == y && acc) True (zip xs ys)

tstartsWith :: Test
tstartsWith =
  "startsWith"
    ~: TestList
      [ "Hello" `startsWith` "Hello World!" ~?= True,
        "Hello" `startsWith` "Wello Horld!" ~?= False
      ]

-- | foldr variant that provides access to each tail of the list
para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para _ b [] = b
para f b (x : xs) = f x xs (para f b xs)

-- | The 'tails' function calculates all suffixes of a give list and returns them
-- in decreasing order of length. For example:
tails :: [a] -> [[a]]
tails = para (\y ys b -> (y : ys) : b) [[]]

ttails :: Test
ttails =
  "tails"
    ~: TestList
      [ "tails0" ~: tails "abc" ~?= ["abc", "bc", "c", ""],
        "tails1" ~: tails "" ~?= [""],
        "tails2" ~: tails "a" ~?= ["a", ""]
      ]

-- | The 'endsWith' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
endsWith :: String -> String -> Bool
endsWith xs = para (\s ss b -> (s : ss) == xs || b) False

tendsWith :: Test
tendsWith =
  "endsWith"
    ~: TestList
      [ "ld!" `endsWith` "Hello World!" ~?= True,
        "World" `endsWith` "Hello World!" ~?= False
      ]

-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
countSub :: String -> String -> Int
countSub xs = para (\s ss b -> fromEnum (xs `startsWith` (s : ss)) + b) (-1)

tcountSub :: Test
tcountSub =
  "countSub"
    ~: TestList
      [ countSub "aa" "aaa" ~?= 2,
        countSub "" "aaac" ~?= 3
      ]

----------------------------------------------------------------------
data Tree a
  = Empty
  | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Branch x t1 t2) = Branch (f x) (mapTree f t1) (mapTree f t2)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ e Empty = e
foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree f z t = foldTree go id t z
  where
    go k l r z0 = l (f k (r z0))

-- The `appendTree` function takes two trees and replaces all of the `Empty`
-- constructors in the first with the second tree.  For example:
-- todo: use fold or map
appendTree :: Tree a -> Tree a -> Tree a
appendTree Empty t = t
appendTree (Branch a l r) t = Branch a (appendTree l t) (appendTree r t)

tappendTree :: Test
tappendTree =
  "appendTree"
    ~: TestList
      [ appendTree (Branch 'a' Empty Empty) (Branch 'b' Empty Empty) ~?= Branch 'a' (Branch 'b' Empty Empty) (Branch 'b' Empty Empty),
        appendTree Empty (Branch 'a' Empty Empty) ~?= Branch 'a' Empty Empty
      ]

-- The `invertTree` function takes a tree of pairs and returns a new tree
-- with each pair reversed.  For example:
invertTree :: Tree (a, b) -> Tree (b, a)
invertTree = mapTree (\(a, b) -> (b, a))

tinvertTree :: Test
tinvertTree =
  "invertTree"
    ~: TestList
      [ invertTree (Branch ("a", True) Empty Empty) ~?= Branch (True, "a") Empty Empty
      ]

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)

-- `takeWhileTree`, applied to a predicate `p` and a tree `t`,
-- returns the largest prefix tree of `t` (possibly empty)
-- where all elements satisfy `p`.
takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree p = foldrTree f Empty
  where
    f x Empty = if p x then Branch x Empty Empty else Empty
    f x t@(Branch y l r) = if p x then Branch y (Branch x l r) Empty else t

ttakeWhileTree :: Test
ttakeWhileTree =
  "takeWhileTree"
    ~: TestList
      [ takeWhileTree (< 3) tree1 ~?= Branch 1 (Branch 2 Empty Empty) Empty,
        takeWhileTree (< 0) tree1 ~?= Empty
      ]

-- `allTree pred tree` returns `False` if any element of `tree`
-- fails to satisfy `pred` and `True` otherwise. For example:
allTree :: (a -> Bool) -> Tree a -> Bool
allTree p = foldrTree (\x acc -> p x && acc) True

tallTree :: Test
tallTree = "allTree" ~: TestList [allTree odd tree1 ~?= False]

-- `map2Tree f xs ys` returns the tree obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one branch is longer than the other, then the extra elements
-- are ignored.
-- ToDo use foldTree
map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree _ _ Empty = Empty
map2Tree _ Empty _ = Empty
map2Tree f t1@(Branch x l1 r1) t2@(Branch y l2 r2) = Branch (f x y) (map2Tree f l1 l2) (map2Tree f r1 r2)

tmap2Tree :: Test
tmap2Tree =
  "map2Tree"
    ~: TestList
      [ map2Tree (+) (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 3 Empty Empty) ~?= Branch 4 Empty Empty
      ]

----------------------------------------------------------------------

main :: IO ()
main = doTests

doTests :: IO ()
doTests = do
  _ <-
    runTestTT $
      TestList
        [ testHO,
          testFoldr,
          testTree
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

testFoldr :: Test
testFoldr =
  TestList
    [ tconcat,
      tstartsWith,
      tendsWith,
      ttails,
      tcountSub
    ]

testTree :: Test
testTree =
  TestList
    [ tappendTree,
      tinvertTree,
      ttakeWhileTree,
      tallTree,
      tmap2Tree
    ]
