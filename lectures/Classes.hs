{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Classes where

import Data.Char (Char)
import qualified Data.List as List
import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))
import Text.Read (Read)
import Prelude hiding (lookup)

data Tree a = Empty | Branch a (Tree a) (Tree a)

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) Empty Empty = True
  (==) (Branch x lx rx) (Branch y ly ry) = x == y && lx == rx && ly == ry
  (==) _ _ = False

tree1, tree2 :: Tree Int
tree1 = Branch 3 (Branch 2 Empty Empty) (Branch 1 Empty Empty)
tree2 = Branch 3 Empty Empty

testTreeEq :: Test
testTreeEq =
  TestList
    [ "tree1 == tree1" ~: tree1 == tree1 ~?= True,
      "tree1 /= tree2" ~: tree1 == tree2 ~?= False,
      "tree1 /= Empty" ~: tree1 == Empty ~?= False
    ]

-- data IntFunctions
--   = OneArg (Int -> Int)
--   | TwoArg (Int -> Int -> Int)
--   deriving (Eq)

data Two a = MkTwo a a deriving (Eq, Show, Read, Ord)

instance Functor Two where
  fmap :: (a -> b) -> Two a -> Two b
  fmap f (MkTwo x y) = MkTwo (f x) (f y)

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ testTreeEq
        ]
  return ()
