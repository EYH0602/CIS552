module Xml where

import Control.Applicative (Alternative (..))
import Parsers (Parser, char, doParse, filter, satisfy, string)
import System.IO
import Prelude hiding (filter)

-- | A simplified datatype for storing XML
data SimpleXML
  = PCDATA String
  | Element ElementName [SimpleXML]
  deriving (Show)

type ElementName = String

reserved :: Char -> Bool
reserved c = c `elem` ['/', '<', '>']

text :: Parser String
text = some $ satisfy $ not . reserved

pcdata :: Parser SimpleXML
pcdata = PCDATA <$> text

emptyContainer :: Parser SimpleXML
emptyContainer = Element <$> (char '<' *> text <* string "/>") <*> pure []

container :: Parser SimpleXML -> Parser SimpleXML
container p =
  Element <$> (char '<' *> text <* char '>')
    <*> many p
    <* (string "</" *> text <* char '>')

xml :: Parser SimpleXML
xml = pcdata <|> emptyContainer <|> container xml

-- | Run a parser on a particular input file
parseFromFile :: Parser a -> String -> IO (Maybe (a, String))
parseFromFile parser filename = do
  handle <- openFile filename ReadMode
  str <- hGetContents handle
  return $ doParse parser str

container2 :: Parser SimpleXML -> Parser SimpleXML
container2 p = (\(x, y, _z) -> Element x y) <$> filter (\(x, _, z) -> x == z) triple
  where
    triple =
      (,,) <$> (char '<' *> text <* string ">")
        <*> many p
        <*> (string "</" *> text <* string ">")
