
-- P505 Autumn 2016, Homework 5, prob12.hs

--
-- imports and helper functions written by staff to make life a little easier
--

import System.IO.Error
import System.IO

maybeRead :: String -> IO (Maybe Int)
maybeRead s = do
  p <- tryIOError (readIO s)
  return (case p of { Left _ -> Nothing ; Right n -> Just n })

rInt :: IO (Maybe Int)
rInt = getLine >>= maybeRead

--
-- Problem 1
--

{-
data InttreeT = EmptyT | NodeT (Int,InttreeT,InttreeT)
data InttreeC = EmptyC | NodeC Int InttreeC InttreeC

insertT EmptyT i = NodeT(i,EmptyT,EmptyT)
insertT (NodeT(j,l,r)) i =
  if i == j 
  then NodeT (j,l,r)
  else if i < j then NodeT(j,insertT l i,r)
  else NodeT(j,l,insertT r i)

fromListT l = foldl insertT EmptyT l

prodT t = undefined

mapT f t = undefined

negateAllT t = undefined

insertC EmptyC i = NodeC i EmptyC EmptyC
insertC (NodeC j l r) i =
  if i == j 
  then NodeC j l r
  else if i < j then NodeC j (insertC l i) r
  else NodeC j l (insertC r i)

fromListC l = foldl insertC EmptyC l

prodC f = undefined

mapC f t = undefined

negateAllC f = undefined
-}

--
-- Problem 2 
--

reverseList (x:xs) = (reverseList xs) ++ [x]
reverseList [] = []

until_pair :: (a -> a -> Bool) -> IO a -> IO [a]
until_pair f action = do { first <- action; addToList first [] }
						where
							addToList new lst = 
								case lst of
									[] -> do { next <- action; addToList next [new] }
									(head:tail) -> if f head new
													then return (reverseList (new:lst))
													else do { next <- action; addToList next (new:lst) }

--
-- A test for until_pair
--

getList :: IO [Int]
getList = until_pair (\x y -> x == y) (rInt >>= (\e -> case e of { Nothing -> return 0; Just n -> return n }))

-- Uncomment out the commented lines below to test until_pair
main :: IO ()
main = do
         putStrLn "Unimplemented"
         putStrLn "put one number on each line until two lines are the same (any line not readable as an Int will be 0";
         xs <- getList;
         putStrLn (show xs)

