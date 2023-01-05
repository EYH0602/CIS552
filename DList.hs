module DList where


type DList a = [a] -> [a]

toList :: DList a -> [a]
toList x = x []

empty :: DList a
empty = id

singleton :: a -> DList a
singleton = (:)

-- append :: [a] -> [a] -> [a] -> [a] -> [a] -> [a]
append :: DList a -> DList a -> DList a
append = (.)

cons :: a -> DList a -> DList a
cons x = append (singleton x)
-- or
-- cons x ys = (singleton x) `append` ys
-- cons = (.) . (:)

fromList :: [a] -> DList a
fromList = foldr cons empty
-- fromList ys = (ys ++)
-- or
-- fromList (x:xs) = cons x (fromList xs)
-- fromList [] = empty


main :: IO ()
main = do
  print (toList empty :: [Int])
  print (toList (singleton 1))
  print $
    let xs = singleton 1
        ys = singleton 2
     in toList (xs `append` ys)
  print $
    let x = "a"
        xs = singleton "b"
     in toList (cons x xs)
  print (toList (fromList [1 .. 5]))

