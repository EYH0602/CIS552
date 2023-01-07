module Main where

-- libraries for Kata problem (only)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Type.Coercion (trans)
import Test.HUnit (Assertion, Test (TestList), assertFailure, runTestTT, (~:), (~?=))
import qualified Text.Read as Read
import Prelude hiding (concat, reverse, zip, (++))

-- Part One

abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || z)

tabc :: Test
tabc =
  "abc"
    ~: TestList
      [ abc True False True ~?= True,
        abc True False False ~?= False,
        abc False True True ~?= False
      ]

-- Part Two

arithmetic :: ((Int, Int), Int) -> ((Int, Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) = (x, y, z)
  where
    x = b * f - c * e
    y = c * d - a * f
    z = a * e - b * d

tarithmetic :: Test
tarithmetic =
  "arithmetic"
    ~: TestList
      [ arithmetic ((1, 2), 3) ((4, 5), 6) ~?= (-3, 6, -3),
        arithmetic ((3, 2), 1) ((4, 5), 6) ~?= (7, -14, 7)
      ]

-- Part Three

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

treverse :: Test
treverse =
  "reverse"
    ~: TestList
      [ reverse [3, 2, 1] ~?= ([1, 2, 3] :: [Int]),
        reverse [1] ~?= ([1] :: [Int])
      ]

-- Part Four

zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

tzip :: Test
tzip =
  "zip"
    ~: TestList
      [ zip "abc" [True, False, True] ~?= [('a', True), ('b', False), ('c', True)],
        zip "abc" [True] ~?= [('a', True)],
        zip [] [] ~?= ([] :: [(Int, Int)])
      ]

--------------------------------------------------------------------------------
-- Problem (List library chops)
--------------------------------------------------------------------------------

-- Part One

-- | The 'minimumMaybe` function computes the mininum value of a
-- nonempty list. If the list is empty, it returns Nothing.
minimumMaybe :: [Int] -> Maybe Int
minimumMaybe = foldr f Nothing
  where
    f x Nothing = Just x
    f x (Just y) = Just $ min x y

tminimumMaybe :: Test
tminimumMaybe =
  "minimumMaybe" ~: TestList [minimumMaybe [] ~?= Nothing, minimumMaybe [2, 1, 3] ~?= Just 1]

-- Part Two

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
startsWith :: String -> String -> Bool
startsWith xs ys = xs == take (length xs) ys

tstartsWith :: Test
tstartsWith =
  "startsWith"
    ~: TestList
      [ "Hello" `startsWith` "Hello World!" ~?= True,
        "Hello" `startsWith` "Wello Horld!" ~?= False
      ]

-- Part Three

-- | The 'endsWith' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
--
-- >>>
-- True
--
-- >>>
-- False
endsWith :: String -> String -> Bool
endsWith xs ys = startsWith (reverse xs) (reverse ys)

tendsWith :: Test
tendsWith =
  "endsWith"
    ~: TestList
      [ "ld!" `endsWith` "Hello World!" ~?= True,
        "World" `endsWith` "Hello World!" ~?= False
      ]

-- Part Four

-- | The 'transpose' function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is *not* the same behavior as the library version
-- of transpose (i.e. the version of transpose from Data.List).
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose xss = map head xss : transpose (map tail xss)

ttranspose :: Test
ttranspose =
  "transpose"
    ~: TestList
      [ transpose [[1, 2, 3], [4, 5, 6]] ~?= [[1, 4], [2, 5], [3, 6]],
        transpose [] ~?= ([] :: [[Int]]),
        transpose [[]] ~?= ([] :: [[Int]]),
        transpose [[3, 4, 5]] ~?= [[3], [4], [5]],
        transpose [[1, 2], [3, 4, 5]] ~?= [[1, 3], [2, 4]]
      ]

-- Part Five

-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
countSub :: String -> String -> Int
countSub "" xs = 1 + length xs
countSub sub xs = length (filter (==sub) (List.subsequences xs)) - 1

tcountSub :: Test
tcountSub =
  "countSub"
    ~: TestList
      [ countSub "aa" "aaa" ~?= 2,
        countSub "" "aaac" ~?= 5
      ]

--------------------------------------------------------------------------------
-- Data Munging Kata
--------------------------------------------------------------------------------

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ testStyle,
          testLists
        ]
  --  testWeather,
  --  testSoccer ]
  return ()

testStyle :: Test
testStyle =
  "testStyle"
    ~: TestList [tabc, tarithmetic, treverse, tzip]

testLists :: Test
testLists =
  "testLists"
    ~: TestList
      [tminimumMaybe, tstartsWith, tendsWith, ttranspose, tcountSub]
