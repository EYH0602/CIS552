module SecretCode where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import System.FilePath (replaceExtension, takeExtension)
import Test.HUnit (Counts, Test (TestList), runTestTT, (~?=))

code :: [(Char, Char)]
code = zip ['a' .. 'z'] cypher ++ zip ['A' .. 'Z'] (map toUpper cypher)
  where
    cypher :: String
    cypher = "thequickbrownfxjmpsvlazydg"

encodeChar :: Char -> Char
encodeChar x = fromMaybe x (lookup x code)

-- case lookup x code of
--   Just y -> y
--   Nothing -> x

encodeLine :: String -> String
encodeLine = map encodeChar

encodeContent :: String -> String
encodeContent = unlines . reverse . map encodeLine . lines

encodeFile :: FilePath -> IO ()
encodeFile f =
  if takeExtension f == "code"
    then putStrLn "Cannot encode .code files"
    else do
      let outFilePath = replaceExtension f "code"
      c <- readFile f
      writeFile outFilePath (encodeContent c)

main :: IO ()
main = do
  print (encodeChar 'a')
  print (encodeChar '.')
  print (encodeLine "abc defgh")
  putStrLn "What file shall I encode?"
  fn <- getLine
  encodeFile fn
  putStrLn "All done!"

testEncodeLine :: IO Counts
testEncodeLine = runTestTT $ TestList [encodeLine "abc defgh" ~?= "the quick"]

testEncodeContent :: IO Counts
testEncodeContent =
  runTestTT $
    encodeContent "abc\n defgh\n" ~?= " quick\nthe\n"
