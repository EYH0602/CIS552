module Basics where

import Test.HUnit (Test, (~?=), runTestTT, Counts)

query :: IO ()
query = do 
  putStr "What is your name? "
  n <- getLine
  putStrLn ("Welcome to CIS 552 " ++ n)
  return ()

t1 :: Test
t1 = (1 + 2 :: Int) ~?= 3

numTest :: IO Counts
numTest = runTestTT t1

main :: IO()
main = query
    -- print (3 * (4 + 5))