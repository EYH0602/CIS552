module MaybePractice where

import qualified Control.Monad as Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Read as Text

data Weather = Weather
  { dayNumber :: Int,
    maxTemp :: Int,
    minTemp :: Int
  }
  deriving (Eq, Show)

parseWeather :: Map String String -> Maybe Weather
parseWeather m = do
  let get s = Map.lookup s m >>= Text.readMaybe :: Maybe Int
  day <- get "day"
  maxtemp <- get "maxTemp"
  mintemp <- get "minTemp"
  return (Weather day maxtemp mintemp)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust m@(Just _) _ = m
firstJust Nothing m = m

sequenceFirst :: Maybe a -> Maybe a -> Maybe a
sequenceFirst m1 m2 = do
  m <- m1
  _ <- m2
  return m
