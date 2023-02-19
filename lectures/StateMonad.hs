module StateMonad where

import Control.Monad (ap, liftM)
import qualified Data.IORef as IO
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
--- definition of the State monad
import qualified State as S

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show, Foldable)

tree :: Tree Char
tree = Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c')

countF :: Tree a -> Int
countF (Leaf _) = 1
countF (Branch t1 t2) = countF t1 + countF t2

countL :: Tree a -> Int
countL = length

countIO :: Tree a -> IO Int
countIO t = do
  -- create a mutable variable, initialize to 0
  count <- IO.newIORef 0
  -- visit every node in the tree, mutating the variable
  let aux (Leaf _) = IO.modifyIORef count (+ 1)
      aux (Branch t1 t2) = aux t1 >> aux t2
  aux t
  -- return the total count
  IO.readIORef count

-- | The number of leaves in the tree that we have currently counted
type Store = Int

countI :: Tree a -> Int
countI t = aux t 0 -- start with 0
  where
    aux :: Tree a -> (Store -> Store)
    aux (Leaf _) = (+ 1) -- we found a leaf
    aux (Branch t1 t2) = \s ->
      let s' = aux t1 s -- pass through in
          s'' = aux t2 s' -- each recursive call
       in s''

labelIO :: Tree a -> IO (Tree (a, Int))
labelIO t = do
  -- create a mutable variable, initialize to 0
  count <- IO.newIORef 0
  -- visit every node in the tree, modifying the variable
  let aux (Leaf x) = do
        c <- IO.readIORef count
        IO.writeIORef count (c + 1)
        return (Leaf (x, c))
      aux (Branch t1 t2) = do
        t1' <- aux t1
        t2' <- aux t2
        return (Branch t1' t2')
  -- traverse and return the tree
  aux t

-- >>> labelIO tree
-- Branch (Branch (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2))

label1 :: Tree a -> Tree (a, Int)
label1 t = fst (aux t 0)
  where
    aux :: Tree a -> Store -> (Tree (a, Int), Store)
    aux (Leaf v) = \s -> (Leaf (v, s), s + 1)
    aux (Branch t1 t2) = \s ->
      let (t1', s') = aux t1 s
          (t2', s'') = aux t2 s'
       in (Branch t1' t2', s'')

-- | Store Transformer
type ST a = Store -> (a, Store)

returnST :: a -> ST a
-- returnST :: a -> Store -> (a, Store)
returnST x s = (x, s)

bindST :: ST a -> (a -> ST b) -> ST b
-- bindST :: (Store -> (a,Store)) -> (a -> (Store -> (b, Store))) -> (Store -> (b, Store))
bindST st f s = st' s
  where
    (x, s') = st s
    st' = f x

label2 :: Tree a -> Tree (a, Int)
label2 t = fst (aux t 0)
  where
    aux :: Tree a -> ST (Tree (a, Int))
    aux (Leaf x) = \s -> (Leaf (x, s), s + 1)
    aux (Branch t1 t2) = \s ->
      let (t1', s') = aux t1 s
          (t2', s'') = aux t2 s'
       in (Branch t1' t2', s'')

newtype ST2 a = S (Store -> (a, Store))

runState :: ST2 a -> (Store -> (a, Store))
runState (S f) = f

instance Monad ST2 where
  return :: a -> ST2 a
  return = pure

  (>>=) :: ST2 a -> (a -> ST2 b) -> ST2 b
  f >>= g = S $ \s ->
    let (a, s') = runState f s
     in runState (g a) s'

instance Functor ST2 where
  fmap :: (a -> b) -> ST2 a -> ST2 b
  fmap = liftM

instance Applicative ST2 where
  pure :: a -> ST2 a
  pure x = S (x,)

  (<*>) :: ST2 (a -> b) -> ST2 a -> ST2 b
  (<*>) = ap

getST2 :: ST2 Store
getST2 = S $ \s -> (s, s)

putST2 :: Store -> ST2 ()
putST2 s = S $ const ((), s)

mlabel' :: Tree a -> ST2 (Tree (a, Int))
mlabel' (Leaf x) = do
  c <- getST2
  putST2 (c + 1)
  return (Leaf (x, c))
mlabel' (Branch t1 t2) = do
  t1' <- mlabel t1
  t2' <- mlabel t2
  return (Branch t1' t2')

mlabel :: Tree a -> ST2 (Tree (a, Int))
mlabel (Leaf x) = getST2 >>= (\c -> putST2 (c + 1) >> return (Leaf (x, c)))
mlabel (Branch t1 t2) = mlabel t1 >>= (\t1' -> mlabel t2 >>= (return . Branch t1'))

label :: Tree a -> Tree (a, Int)
label t = fst (runState (mlabel t) 0)

mlabelS :: Tree t -> S.State Int (Tree (t, Int))
mlabelS (Leaf x) = do
  c <- S.get
  S.put (c + 1)
  return (Leaf (x, c))
mlabelS (Branch t1 t2) = do
  t1' <- mlabelS t1
  t2' <- mlabelS t2
  return (Branch t1' t2')

data MySt a = M
  { index :: Int,
    freq :: Map a Int
  }
  deriving (Eq, Show)

updateIndexM :: S.State (MySt a) Int
updateIndexM = do
  m <- S.get
  let i = index m
  S.put (m {index = i + 1}) -- create a new record like m, but index as given
  return i

updFreqM :: Ord a => a -> S.State (MySt a) ()
updFreqM x = do
  m <- S.get
  let f = freq m
  let v = Maybe.fromMaybe 0 (Map.lookup x f)
  S.put (m {freq = Map.insert x (v + 1) f})
  return ()

mlabelM :: Ord a => Tree a -> S.State (MySt a) (Tree (a, Int))
mlabelM (Leaf x) = do
  c <- updateIndexM
  updFreqM x
  return (Leaf (x, c))
mlabelM (Branch t1 t2) = do
  t1' <- mlabelM t1
  t2' <- mlabelM t2
  return (Branch t1' t2')

initM :: MySt a
initM = M 0 Map.empty

tree2 :: Tree Char
tree2 = Branch tree tree

lt :: Tree (Char, Int)
s :: MySt Char
(lt, s) = S.runState (mlabelM tree2) initM

main = do
  print lt
  print s
