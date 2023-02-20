{-# LANGUAGE ScopedTypeVariables #-}

module RandomGen where

import Control.Monad
import qualified State as S
import System.Random (StdGen)
import qualified System.Random as Random (mkStdGen, randomIO, uniform, uniformR)

-- | Returns an Int that is uniformly distributed in a range of at least 30 bits.
uniformInt :: StdGen -> (Int, StdGen)
uniformInt = Random.uniform

-- | Returns True / False with even chances
uniformBool :: StdGen -> (Bool, StdGen)
uniformBool = Random.uniform

mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

testRandom :: Int -> Int
testRandom i = fst (uniformInt (mkStdGen i))

nextBounded :: Int -> StdGen -> (Int, StdGen)
nextBounded bound s = let (x, s1) = uniformInt s in (x `mod` bound, s1)

testBounded :: Int -> Int
testBounded = fst . nextBounded 20 . mkStdGen

-- | Extract random values of any type
class Arb1 a where
  arb1 :: StdGen -> (a, StdGen)

instance Arb1 Int where
  arb1 = uniformInt

instance Arb1 Bool where
  arb1 = uniformBool

testArb1 :: Arb1 a => Int -> a
testArb1 = fst . arb1 . mkStdGen

instance (Arb1 a, Arb1 b) => Arb1 (a, b) where
  arb1 :: StdGen -> ((a, b), StdGen)
  arb1 s = ((a, b), s2)
    where
      (a :: a, s1) = arb1 s
      (b :: b, s2) = arb1 s1

instance (Arb1 a) => Arb1 (Maybe a) where
  arb1 :: StdGen -> (Maybe a, StdGen)
  arb1 s = (Just a, s1)
    where
      (a :: a, s1) = arb1 s

instance Arb1 a => Arb1 [a] where
  arb1 :: Arb1 a => StdGen -> ([a], StdGen)
  arb1 s = foldr gen ([], s1) (replicate n 0)
    where
      (n :: Int, s1) = nextBounded 10 s
      gen _ (acc, s') = let (a :: a, s'') = arb1 s' in (a : acc, s'')

type Gen a = S.State StdGen a

class Arb a where
  arb :: Gen a

instance Arb Int where
  arb :: Gen Int
  arb = do
    s <- S.get
    let (y :: Int, s') = Random.uniform s
    S.put s'
    return y

bounded :: Int -> Gen Int
bounded b = do
  x :: Int <- arb
  return (x `mod` b)

sample :: Show a => Gen a -> IO ()
sample gen = do
  seed <- (Random.randomIO :: IO Int) -- get a seed from the global random number generator
  -- hidden in the IO monad
  let vals = S.evalState (replicateM 10 gen) (mkStdGen seed)
  forM_ vals print

instance (Arb a, Arb b) => Arb (a, b) where
  arb :: (Arb a, Arb b) => Gen (a, b)
  arb = (,) <$> arb <*> arb

-- arb = do
--   (a :: a) <- arb
--   (b :: b) <- arb
--   return (a, b)

elements :: [a] -> Gen a
elements xs = bounded (length xs) >>= (\b -> return (xs !! b))

instance Arb Bool where
  arb :: Gen Bool
  arb = elements [False, True]

frequency :: [(Int, Gen a)] -> Gen a
frequency table = bounded total >>= (`nth` table)
  where
    total = sum (map fst table)

nth :: Int -> [(Int, a)] -> a
nth i ((j, x) : xs)
  | i < j = x
  | otherwise = nth (i - j) xs
nth _ [] = error "frequency: empty"

instance (Arb a) => Arb [a] where
  arb :: Arb a => Gen [a]
  arb = frequency [(1, return []), (3, (:) <$> arb <*> arb)]
