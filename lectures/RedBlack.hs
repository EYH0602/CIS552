{-# LANGUAGE TemplateHaskell #-}

module RedBlack where

import qualified Data.Foldable as Foldable
import Test.QuickCheck hiding (elements)

data Color = R | B deriving (Eq, Show)

data T a = E | N Color (T a) a (T a) deriving (Eq, Show, Foldable)

newtype RBT a = Root (T a) deriving (Show, Foldable)

-- | List all of the elements of the finite set, in ascending order
elements :: RBT a -> [a]
elements = Foldable.toList

instance Eq a => Eq (RBT a) where
  t1 == t2 = elements t1 == elements t2

-- | access the color of the tree
color :: T a -> Color
color (N c _ _ _) = c
color E = B

-- | calculate the black height of the tree
blackHeight :: T a -> Int
blackHeight E = 1
blackHeight (N c a _ _) = blackHeight a + (if c == B then 1 else 0)

-- | Create an RBT by blackening the top node (if necessary)
blacken :: T a -> RBT a
blacken E = Root E
blacken (N _ l v r) = Root (N B l v r)

empty :: RBT a
empty = Root E

member :: Ord a => a -> RBT a -> Bool
member x (Root t) = bstSearch x t

insert :: Ord a => a -> RBT a -> RBT a
insert x (Root t) = blacken (ins x t)

delete :: Ord a => a -> RBT a -> RBT a
delete x (Root t) = blacken (del x t)

---------------- Tree functions --------------
bstSearch :: Ord a => a -> T a -> Bool
bstSearch _ E = False
bstSearch x (N _ l v r)
  | x < v = bstSearch x l
  | x > v = bstSearch x r
  | otherwise = True

ins :: Ord a => a -> T a -> T a
ins x E = N R E x E
ins x t@(N c l v r)
  | x < v = balance (N c (ins x l) v r)
  | x > v = balance (N c l v (ins x r))
  | otherwise = t

balance :: T a -> T a
balance (N B (N R (N R a x b) y c) z d) = N R (N B a x b) y (N B c z d)
balance (N B (N R a x (N R b y c)) z d) = N R (N B a x b) y (N B c z d)
balance (N B a x (N R (N R b y c) z d)) = N R (N B a x b) y (N B c z d)
balance (N B a x (N R b y (N R c z d))) = N R (N B a x b) y (N B c z d)
balance t = t

del :: Ord a => a -> T a -> T a
del _ E = E
del x (N _ a y b)
  | x < y = delLeft a y b
  | x > y = delRight a y b
  | otherwise = merge a b
  where
    delLeft c@(N B _ _ _) z d = balLeft (del x c) z d
    delLeft c z d = N R (del x c) z d

    delRight c z d@(N B _ _ _) = balRight c z (del x d)
    delRight c z d = N R c z (del x d)

balLeft :: T a -> a -> T a -> T a
balLeft (N R a x b) y c = N R (N B a x b) y c
balLeft bl x (N B a y b) = balance (N B bl x (N R a y b))
balLeft bl x (N R (N B a y b) z c) =
  N R (N B bl x a) y (balance (N B b z (redden c)))
balLeft _ _ _ = error "invariant violation"

-- reduce the black height of the subtree c reddening the node
redden :: T a -> T a
redden (N B a x b) = N R a x b
redden _ = error "invariant violation"

balRight :: T a -> a -> T a -> T a
balRight a x (N R b y c) = N R a x (N B b y c)
balRight (N B a x b) y bl = balance (N B (N R a x b) y bl)
balRight (N R a x (N B b y c)) z bl =
  N R (balance (N B (redden a) x b)) y (N B c z bl)
balRight _ _ _ = error "invariant violation"

merge :: T a -> T a -> T a
merge E x = x
merge x E = x
merge (N R a x b) (N R c y d) =
  case merge b c of
    N R b' z c' -> N R (N R a x b') z (N R c' y d)
    bc -> N R a x (N R bc y d)
merge (N B a x b) (N B c y d) =
  case merge b c of
    N R b' z c' -> N R (N B a x b') z (N B c' y d)
    bc -> balLeft a x (N B bc y d)
merge a (N R b x c) = N R (merge a b) x c
merge (N R a x b) c = N R a x (merge b c)

------------------- Examples -----------------

good1 :: RBT Int
good1 = Root $ N B (N B E 1 E) 2 (N B E 3 E)

-- Here is one with a red Root (violates invariant 2).
bad1 :: RBT Int
bad1 = Root $ N R (N B E 1 E) 2 (N B E 3 E)

-- Here's one that violates the black height requirement (invariant 3).
bad2 :: RBT Int
bad2 = Root $ N B (N R E 1 E) 2 (N B E 3 E)

-- Now define a red-black tree that violates invariant 4.
bad3 :: RBT Int
bad3 = Root $ N B (N R (N R E 1 E) 2 E) 3 E

-- Now define a red-black tree that isn't a binary search tree
-- (i.e. the values stored in the tree are not in strictly increasing order).
bad4 :: RBT Int
bad4 = Root $ N B (N B E 3 E) 2 (N B E 4 E)

trees :: [(String, RBT Int)]
trees =
  [ ("good1", good1),
    ("bad1", bad1),
    ("bad2", bad2),
    ("bad3", bad3),
    ("bad4", bad4),
    ("empty", empty)
  ]

-- binary search
isBST :: Ord a => RBT a -> Bool
isBST (Root t) = aux t
  where
    aux E = True
    aux (N _ l k r) =
      aux l && aux r
        && all (< k) (elements (Root l))
        && all (> k) (elements (Root r))

-- | A red-black tree is a BST if an in-order traversal is strictly ordered.
isBST' :: Ord a => RBT a -> Bool
isBST' = orderedBy (<) . elements

-- | Are the elements in the list ordered by the provided operation?
orderedBy :: (a -> a -> Bool) -> [a] -> Bool
orderedBy op (x : y : xs) = x `op` y && orderedBy op (y : xs)
orderedBy _ _ = True

prop_isBST_isBST' :: Ord a => RBT a -> Property
prop_isBST_isBST' t = property (isBST t == isBST' t)

isRootBlack :: RBT a -> Bool
isRootBlack (Root t) = color t == B

consistentBlackHeight :: RBT a -> Bool
consistentBlackHeight (Root E) = True
consistentBlackHeight (Root (N _ l _ r)) = blackHeight l == blackHeight r

noRedRed :: RBT a -> Bool
noRedRed (Root t) = aux t
  where
    aux (N R a _ b) = color a == B && color b == B && aux a && aux b
    aux (N B a _ b) = aux a && aux b
    aux E = True

------------------ Tests --------------------
testProps :: IO ()
testProps = mapM_ checkTree trees
  where
    checkTree (name, tree) = do
      putStrLn $ "******* Checking " ++ name ++ " *******"
      quickCheck $ once (counterexample "RB2" $ isRootBlack tree)
      quickCheck $ once (counterexample "RB3" $ consistentBlackHeight tree)
      quickCheck $ once (counterexample "RB4" $ noRedRed tree)
      quickCheck $ once (counterexample "BST" $ isBST tree)

type A = Small Int

prop_Valid :: RBT A -> Property
prop_Valid tree =
  counterexample "RB2" (isRootBlack tree)
    .&&. counterexample "RB3" (consistentBlackHeight tree)
    .&&. counterexample "RB4" (noRedRed tree)
    .&&. counterexample "BST" (isBST tree)

instance (Ord a, Arbitrary a) => Arbitrary (RBT a) where
  arbitrary :: Gen (RBT a)
  arbitrary = foldr insert empty <$> (arbitrary :: Gen [a])

  shrink :: RBT a -> [RBT a]
  shrink (Root E) = []
  shrink (Root (N _ l _ r)) = [blacken l, blacken r]

prop_DeleteValid :: RBT A -> A -> Property
prop_DeleteValid t x = prop_Valid (delete x t)

prop_ShrinkValid :: RBT A -> Property
prop_ShrinkValid t = conjoin (map prop_Valid (shrink t))

prop_InsertEmpty :: A -> Bool
prop_InsertEmpty x = elements (insert x empty) == [x]

prop_InsertInsert :: A -> A -> RBT A -> Bool
prop_InsertInsert x y t =
  insert x (insert y t) == insert y (insert x t)

prop_InsertDelete :: A -> A -> RBT A -> Bool
prop_InsertDelete k k0 t =
  insert k (delete k0 t)
    == if k == k0 then insert k t else delete k0 (insert k t)

prop_DeleteEmpty :: A -> Bool
prop_DeleteEmpty x = delete x empty == empty

prop_DeleteInsert :: A -> A -> RBT A -> Bool
prop_DeleteInsert k k0 t =
  delete k (insert k0 t)
    == if k == k0
      then t
      else insert k0 (delete k t)

prop_DeleteDelete :: A -> A -> RBT A -> Bool
prop_DeleteDelete x y t =
  delete x (delete y t) == delete y (delete x t)

prop_MemberEmpty :: A -> Bool
prop_MemberEmpty x = not (member x empty)

prop_MemberInsert :: A -> A -> RBT A -> Bool
prop_MemberInsert k k0 t =
  member k (insert k0 t) == (k == k0 || member k t)

prop_MemberDelete :: A -> A -> RBT A -> Bool
prop_MemberDelete k k0 t =
  member k (delete k0 t) == (k /= k0 && member k t)

{-
Using the TemplateHaskell extension,
the following code below defines an operation that will invoke QuickCheck
with all definitions that start with prop_ above.
-}
return []

main :: IO Bool
main = $quickCheckAll
