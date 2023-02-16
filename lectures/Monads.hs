module Monads where

import Control.Monad (guard)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Prelude hiding ((>>))

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

-- | zip two trees together
zipTree :: Tree a -> Tree b -> Tree (a, b)
zipTree (Leaf x) (Leaf y) = Leaf (x, y)
zipTree (Branch a b) (Branch c d) = Branch (zipTree a c) (zipTree b d)

testZip0 :: Bool
testZip0 =
  zipTree
    (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))
    (Branch (Leaf (0 :: Int)) (Branch (Leaf 1) (Leaf 2)))
    == Branch (Leaf ("a", 0)) (Branch (Leaf ("b", 1)) (Leaf ("c", 2)))

zipTree1 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree1 (Leaf x) (Leaf y) = Just $ Leaf (x, y)
zipTree1 (Leaf _) _ = Nothing
zipTree1 _ (Leaf _) = Nothing
zipTree1 (Branch a b) (Branch c d) = do
  l <- zipTree1 a c
  r <- zipTree1 b d
  return (Branch l r)

testZip :: (Tree String -> Tree Int -> Maybe (Tree (String, Int))) -> Bool
testZip zt =
  zt
    (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))
    (Branch (Leaf 0) (Branch (Leaf 1) (Leaf 2)))
    == Just (Branch (Leaf ("a", 0)) (Branch (Leaf ("b", 1)) (Leaf ("c", 2))))
    && Maybe.isNothing (zt (Branch (Leaf "a") (Leaf "b")) (Leaf 0))

bind :: Maybe t -> (t -> Maybe a) -> Maybe a
bind x f = case x of
  Nothing -> Nothing
  Just y -> f y

zipTree3 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree3 (Leaf a) (Leaf b) = return (Leaf (a, b))
zipTree3 (Branch l r) (Branch l' r') =
  zipTree3 l l'
    `bind` ( \x ->
               zipTree3 r r' `bind` \y -> return (Branch x y)
           )
zipTree3 _ _ = Nothing

zipTree4 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree4 (Leaf a) (Leaf b) = return (Leaf (a, b))
zipTree4 (Branch l r) (Branch l' r') =
  zipTree3 l l'
    >>= ( \x ->
            zipTree3 r r'
              >>= ( \y ->
                      return (Branch x y)
                  )
        )
zipTree4 _ _ = Nothing

zipTree5 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree5 (Leaf a) (Leaf b) = return (Leaf (a, b))
zipTree5 (Branch l r) (Branch l' r') = do
  nl <- zipTree5 l l'
  nr <- zipTree5 r r'
  return (Branch nl nr)
zipTree5 _ _ = Nothing

(>>) :: Monad m => m a -> m b -> m b
m1 >> m2 = m1 >>= const m2

zipTree6 :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree6 (Leaf a) (Leaf b) = pure (Leaf (a, b))
zipTree6 (Branch l r) (Branch l' r') =
  pure Branch <*> zipTree6 l l' <*> zipTree6 r r'
zipTree6 _ _ = Nothing

fmapMonad :: (Monad m) => (a -> b) -> m a -> m b
fmapMonad f m = m >>= return . f

pureMonad :: (Monad m) => a -> m a
pureMonad = return

zapMonad :: (Monad m) => m (a -> b) -> m a -> m b
zapMonad m1 m2 = m1 >>= \f -> m2 >>= \x -> return (f x)

---- List ----
pairs0 :: [a] -> [b] -> [(a, b)]
pairs0 xs ys = concat (map (\x -> concat (map (\y -> [(x, y)]) ys)) xs)

testPairs :: ([Int] -> [Int] -> [(Int, Int)]) -> Bool
testPairs ps =
  ps [1, 2, 3, 4] [5, 6, 7, 8]
    == [ (1, 5),
         (1, 6),
         (1, 7),
         (1, 8),
         (2, 5),
         (2, 6),
         (2, 7),
         (2, 8),
         (3, 5),
         (3, 6),
         (3, 7),
         (3, 8),
         (4, 5),
         (4, 6),
         (4, 7),
         (4, 8)
       ]

pairs1 :: [a] -> [b] -> [(a, b)]
pairs1 xs ys = concatMap (\x -> concatMap (\y -> [(x, y)]) ys) xs

pairs2 :: [a] -> [b] -> [(a, b)]
pairs2 xs ys = xs >>= (\x -> ys >>= (\y -> [(x, y)]))

pairs3 :: [a] -> [b] -> [(a, b)]
pairs3 xs ys = do
  x <- xs
  y <- ys
  return (x, y)

pairs4 :: [Int] -> [Int] -> [(Int, Int)]
pairs4 xs ys = [(x, y) | x <- xs, y <- ys]

pairs5 :: [Int] -> [Int] -> [(Int, Int)]
pairs5 xs ys = [(x, y) | x <- xs, y <- ys, x /= y]

pairs5' :: [Int] -> [Int] -> [(Int, Int)]
pairs5' xs ys = do
  x <- xs
  y <- ys
  guard (x /= y) --- remember that no `<-` means `>>`, i.e. `>>=` ignoring the argument
  return (x, y)

-- >>> map' (+1) [1,2,3]
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- >>> firstLess [1,2,3] [1,2,3]
firstLess :: Ord a => [a] -> [a] -> [(a, a)]
firstLess xs ys = xs >>= (\x -> ys >>= (\y -> guard (x < y) >> [(x, y)]))

map1 :: (a -> b) -> [a] -> [b]
map1 f xs = do
  x <- xs
  return (f x)

firstLess1 :: Ord a => [a] -> [a] -> [(a, a)]
firstLess1 xs ys = do
  x <- xs
  y <- ys
  guard (x < y)
  return (x, y)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = do
  x <- xs
  guard (f x)
  return x

pairs6 :: [a] -> [b] -> [(a, b)]
pairs6 xs ys = pure (,) <*> xs <*> ys

pairs7 :: [a] -> [b] -> [(a, b)]
pairs7 xs ys = fmap (,) xs <*> ys
