-- obligatory main
module Main where
import Kata(testParseEventChar)
import SortedList(testListMonoid, testSortedList,
                  testMinimum, testNumDistinct, testCount)
import MergeSort(testSortedFromList,testSortedFromList',
                 testSumOfProducts,testCrispy, testDivide, testDivideList)
import Test.HUnit ( runTestTT, Test(TestList) )

main :: IO ()
main = do
  _ <- runTestTT $ TestList [testParseEventChar]
  _ <- runTestTT $ TestList [testListMonoid, testSortedList, testMinimum, testNumDistinct, testCount]
  _ <- runTestTT $ TestList [testSortedFromList, testSortedFromList', testSumOfProducts,
                             testCrispy, testDivide, testDivideList]
  return ()