module Queue where

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import Test.QuickCheck

data Q a = Q [a] [a] deriving (Show, Eq)

empty :: Q a
empty = Q [] []

enq :: Q a -> a -> Q a
enq (Q b f) x = Q (x : b) f

deq :: Q a -> Maybe (a, Q a)
deq (Q [] []) = Nothing
deq (Q b []) = deq $ Q [] (reverse b)
deq (Q b (x : f)) = Just (x, Q b f)

fromList :: [a] -> Q a
fromList = foldl enq empty

toList :: Q a -> [a]
toList q = case deq q of
  Just (x, q') -> x : toList q'
  Nothing -> []

----------- Tests ----------------
prop_deq_empty :: Property
prop_deq_empty = property $ Maybe.isNothing (deq empty)

prop_queue_reflexive :: [Int] -> Property
prop_queue_reflexive xs = property $ xs == (toList . fromList) xs

prop_deq_reflexive :: [Int] -> Bool
prop_deq_reflexive [] = True
prop_deq_reflexive xs = head xs == poped && tail xs == toList rest
  where
    q = fromList xs
    (poped, rest) = Maybe.fromMaybe (0, empty) (deq q)

prop_ordered :: [Int] -> Bool
prop_ordered xs = xs == toList (fromList xs :: Q Int)

instance Arbitrary a => Arbitrary (Q a) where
  arbitrary = do
    front <- listOf arbitrary
    back <- listOf arbitrary
    return (Q front back)
  shrink (Q f b) =
    map (Q f) (shrink b) ++ map (`Q` b) (shrink f)

main :: IO ()
main = do
  quickCheck prop_deq_empty
  quickCheck prop_ordered
  quickCheck prop_queue_reflexive
  quickCheck prop_deq_reflexive
