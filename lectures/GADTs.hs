-- Allow data to be used in types
{-# LANGUAGE DataKinds #-}
-- Constrain datatypes via indices
{-# LANGUAGE GADTs #-}
-- Allow kind annotations on types
{-# LANGUAGE KindSignatures #-}
-- Bind polymorphic type variables
{-# LANGUAGE ScopedTypeVariables #-}
-- Kind annotations like type annotations
{-# LANGUAGE StandaloneKindSignatures #-}
-- Use 'Type' instead of '*' in kinds
{-# LANGUAGE NoStarIsType #-}

module GADTs where

import Data.Kind (Type)
import Test.HUnit (Test, (~?=))

data Flag = Empty | NonEmpty

data List :: Flag -> Type -> Type where
  Nil :: List Empty a
  Cons :: a -> List f a -> List NonEmpty a

ex0 :: List 'Empty Int
ex0 = Nil

ex1 :: List 'NonEmpty Int
ex1 = Cons 1 (Cons 2 (Cons 3 Nil))

safeHd :: List NonEmpty a -> a
safeHd (Cons h _) = h

foldr' :: (a -> b -> b) -> b -> List f a -> b
foldr' _ acc Nil = acc
foldr' f acc (Cons x xs) = f x (foldr' f acc xs)

reduce' :: (a -> a -> a) -> List NonEmpty a -> a
reduce' _ (Cons x Nil) = x
reduce' f (Cons x xs@(Cons _ _)) = f x (reduce' f xs)

map' :: (a -> b) -> List f a -> List f b
map' f Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

data OldList :: Type -> Type where
  OL :: List f a -> OldList a

isNonempty :: OldList a -> Maybe (List NonEmpty a)
isNonempty (OL Nil) = Nothing
isNonempty (OL (Cons x xs)) = Just (Cons x xs)

filter' :: (a -> Bool) -> List f a -> OldList a
filter' f (Cons x xs) =
  case filter' f xs of
    OL xs' ->
      if f x
        then OL (Cons x xs')
        else OL xs'
filter' f Nil = OL Nil
