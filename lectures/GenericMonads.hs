module GenericMonads where

import qualified Data.Char as Char
import Test.HUnit
import Prelude hiding (mapM, sequence)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x : xs) = do
  b <- f x
  bs <- mapM f xs
  return (b : bs)

maybeUpper :: Char -> Maybe Char
maybeUpper x = if Char.isAlpha x then Just (Char.toUpper x) else Nothing

onlyUpper :: [Char] -> [Char]
onlyUpper = filter Char.isUpper

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f acc [x] = f acc x
foldM f acc (x : xs) = f acc x >>= (\a -> foldM f a xs)

testFoldM :: Test
testFoldM =
  TestList
    [ addEven [1, 2, 3] ~=? Nothing,
      addEven [2, 4] ~=? Just 6,
      foldM safeDiv 16 [2, 2] ~=? Just 4,
      foldM (flip replicate) 'a' [3, 2] ~=? "aaaaaa"
    ]

addEven :: [Int] -> Maybe Int
addEven = foldM f 0
  where
    f x y
      | even x = Just (x + y)
      | otherwise = Nothing

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = if y == 0 then Nothing else Just (x `div` y)

crazy :: [Int] -> [Int]
crazy = foldM f 0
  where
    f x y
      | even x = [x, y]
      | otherwise = [y]

sequence :: Monad m => [m a] -> m [a]
sequence = foldr k (return [])
  where
    k m m' = do
      x <- m
      xs <- m'
      return (x : xs)

testSequence :: Test
testSequence =
  TestList
    [ sequence [Just (3 :: Int), Nothing, Just 4] ~=? Nothing,
      sequence [[1 :: Int, 2], [3], [4, 5]] ~=? [[1, 3, 4], [1, 3, 5], [2, 3, 4], [2, 3, 5]],
      sequence (map maybeUpper "abcd") ~=? Just "ABCD",
      sequence (map maybeUpper "abcd2") ~=? Nothing
    ]

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g x = f x >>= g

testKleisli :: Test
testKleisli =
  TestList
    [ (maybeUpper >=> earlyOrd) 'a' ~=? Just 65,
      (maybeUpper >=> earlyOrd) '2' ~=? Nothing,
      (mDup >=> mDup) 1 ~=? [1, 1, 1, 1],
      (replicate 2 >=> replicate 3) 'l' ~=? "llllll"
    ]

mDup :: Int -> [Int]
mDup x = [x, x]

earlyOrd :: Char -> Maybe Int
earlyOrd c = if c < 'm' then Just (Char.ord c) else Nothing

join :: (Monad m) => m (m a) -> m a
join x = x >>= id

testJoin :: Test
testJoin =
  TestList
    [ join [[1 :: Int, 2], [3, 4]] ~=? [1, 2, 3, 4],
      join [[1 :: Int, 2], [3, 4], []] ~=? [1, 2, 3, 4],
      join (Just (Just (3 :: Int))) ~=? Just 3
    ]

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM = fmap

testLiftM :: Test
testLiftM =
  TestList
    [ liftM not (Just True) ~=? Just False,
      liftM not [True, False] ~=? [False, True]
    ]

liftM2 :: (Monad m) => (a -> b -> r) -> m a -> m b -> m r
liftM2 f m1 m2 = do
  x <- m1
  y <- m2
  return (f x y)

testLiftM2 :: Test
testLiftM2 =
  TestList
    [ liftM2 (+) (Just (1 :: Int)) (Just 2) ~=? Just 3,
      liftM2 (+) [1, 2] [3, 4 :: Int] ~=? [4, 5, 5, 6]
    ]

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f = foldr ((<*>) . fmap (:) . f) (pure [])

foldA :: Applicative f => (a -> b -> f a) -> a -> [b] -> f a
foldA = undefined

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

kleisliA :: Applicative f => (a -> f b) -> (b -> f c) -> a -> f c
kleisliA = undefined

joinA :: (Applicative f) => f (f a) -> f a
joinA = undefined

liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA f x = f <$> x

liftA2 :: (Applicative f) => (a -> b -> r) -> f a -> f b -> f r
liftA2 f x y = f <$> x <*> y

main :: IO Counts
main =
  runTestTT $
    TestList
      [ testFoldM,
        testSequence,
        testKleisli,
        testJoin,
        testLiftM,
        testLiftM2
      ]
