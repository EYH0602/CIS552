module TreeFolds where

import qualified Data.DList as DL
import Datatypes (Tree (..), exTree, infixOrder)
import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))

testInfixOrder :: Test
testInfixOrder = "infixOrder" ~: infixOrder exTree ~?= [1, 2, 4, 5, 9, 7]

-- | A big "right-skewed" tree
bigRightTree :: Int -> Tree Int
bigRightTree m = go 0
  where
    go n = if n <= m then Branch n Empty (go (n + 1)) else Empty

-- | A big "left-skewed" tree
bigLeftTree :: Int -> Tree Int
bigLeftTree m = go 0
  where
    go n = if n <= m then Branch n (go (n + 1)) Empty else Empty

infixOrder1 :: Tree a -> [a]
infixOrder1 t = DL.toList $ tree2DList t
  where
    tree2DList :: Tree a -> DL.DList a
    tree2DList Empty = DL.empty
    tree2DList (Branch x l r) = DL.concat [tree2DList l, DL.singleton x, tree2DList r]

tinfixOrder1 :: Test
tinfixOrder1 = "infixOrder1a" ~: infixOrder1 exTree ~?= [1, 2, 4, 5, 9, 7]

infixOrder2 :: Tree Int -> [Int]
infixOrder2 t = aux t []
  where
    aux :: Tree Int -> [Int] -> [Int]
    aux Empty z = z
    aux (Branch x l r) z = aux l (x : aux r z)

-- Let's generalize the "base case" and "inductive step" of the definition above,
-- separating the recursion from the specific operation of traversal.
-- First, we identify these operators inside the definition of infixOrder.
infixOrder3 :: Tree Int -> [Int]
infixOrder3 t = aux t b
  where
    b = []
    f = (:)
    aux Empty z = z
    aux (Branch x l r) z = aux l (f x (aux r z))

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree f b t = aux t b
  where
    aux Empty z = z
    aux (Branch x l r) z = aux l (f x (aux r z))

infixOrder4 :: Tree a -> [a]
infixOrder4 = foldrTree (:) []

sizeTree :: Tree Int -> Int
sizeTree = foldrTree (const (1 +)) 0

sumTree :: Tree Int -> Int
sumTree = foldrTree (+) 0

anyTree :: (a -> Bool) -> Tree a -> Bool
anyTree f = foldrTree (\x b -> f x || b) False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f = foldrTree (\x b -> f x && b) True

foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree f b t = aux t b
  where
    f' = flip f
    aux Empty z = z
    aux (Branch x l r) z = aux r (f' x (aux l z))

revOrder :: Tree a -> [a]
revOrder = foldlTree (flip (:)) []

trevOrder :: Test
trevOrder = "revOrder" ~: revOrder exTree ~?= [7, 9, 5, 4, 2, 1]

-- Define foldrTree and foldlTree in terms of foldTree.
foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ e Empty = e
foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

foldrTree' :: (a -> b -> b) -> b -> Tree a -> b
foldrTree' f z t = foldTree go id t z
  where
    go k l r z0 = l (f k (r z0))

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)

tfoldrTree' :: Test
tfoldrTree' = "foldrTree'" ~: foldrTree' (+) 0 tree1 ~?= 6

foldlTree' :: (b -> a -> b) -> b -> Tree a -> b
foldlTree' f z t = foldTree go id t z
  where
    go k l r z0 = r (f (l z0) k)

tfoldlTree' :: Test
tfoldlTree' = "foldlTree'" ~: foldlTree' (+) 0 tree1 ~?= 6

main :: IO ()
main = runTests

runTests :: IO ()
runTests = do
  _ <-
    runTestTT $
      TestList
        [ testInfixOrder,
          tinfixOrder1,
          trevOrder,
          tfoldrTree',
          tfoldlTree'
        ]
  return ()
