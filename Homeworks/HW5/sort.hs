
-- P505 Autumn 2016, Homework 5, sort.hs

--
-- imports and helper functions written by staff to make life a little easier
--

import System.IO.Error
import System.IO
import System.Environment
import System.CPUTime
import Data.List
import Text.Printf

maybeRead :: String -> IO (Maybe Int)
maybeRead s = do
  p <- tryIOError (readIO s)
  return (case p of { Left _ -> Nothing ; Right n -> Just n })

rInt :: IO (Maybe Int)
rInt = getLine >>= maybeRead

--
-- Problem 3
--

{-
selectionSort :: [Int] -> [Int]
selectionSort xs = undefined

mergeSort :: [Int] -> [Int]
mergeSort xs = undefined

topK :: Int -> ([Int] -> [Int]) -> [Int] -> [Int]
topK k sorter xs = take k (sorter xs)

readIntFile :: Handle -> IO [Int]
readIntFile hHandle = readAll
  where
    readAll = do
      isEof <- hIsEOF hHandle
      case isEof of
        True -> return []
        False -> do
          line <- hGetLine hHandle
          parsed <- maybeRead line
          case parsed of
            Nothing -> return []
            Just n -> readAll >>= (return . ((:) n))

time :: IO t -> IO Double
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime 
    let diff = (fromIntegral (end - start)) / (10^12)
    return diff

forceList = foldr seq ()

doSort :: IO ()
doSort = do
  args <- getArgs
  case args of
    [] -> putStrLn "No input"
    x:xs -> do

      -- get input
      h <- openFile x ReadMode
      iList <- readIntFile h
      hClose h
      putStrLn "Done reading file"

      -- first print top-k results for testing purposes
      print (show (topK 5 selectionSort iList))
      print (show (topK 5 mergeSort iList))

      -- now time things using Haskell's seq to force evaluation while
      -- not throwing off timing by actually printing results
      selectTime <- time (forceList (topK 5 selectionSort iList) `seq` return ())
      mergeTime  <- time (forceList (topK 5 mergeSort iList) `seq` return ())
      printf "Selection sort time %0.3f\n" selectTime
      printf "Merge sort time %0.3f\n" mergeTime
      return ()
-}
--
-- Problem 4
--

readAndPrintSorted :: IO ()
readAndPrintSorted = helper []
						where 
							helper memory =
								do
									input <- rInt;
									case input of
										Nothing -> return ()
										Just n -> do
													putStrLn (show (Data.List.sort (n:memory)))
													helper (n:memory)

--
-- main
-- change this to readAndPrintSorted to test Problem 4
--
main :: IO ()
main = readAndPrintSorted
