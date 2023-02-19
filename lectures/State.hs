module State (State, get, put, modify, runState, evalState, execState) where

import Control.Monad (ap, liftM)

newtype State s a = S (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (S f) = f

instance Monad (State s) where
  return :: a -> State s a
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f = S $ \s -> let (a, s') = runState st s in runState (f a) s'

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure x = S (x,) -- this tuple section (x,) is equivalent to \y -> (x,y)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) = ap

evalState :: State s a -> s -> a
evalState st = fst . runState st

execState :: State s a -> s -> s
execState st = snd . runState st

get :: State s s
get = S $ \s -> (s, s)

put :: s -> State s ()
put s' = S $ const ((), s')

modify :: (s -> s) -> State s ()
modify f = get >>= put . f
