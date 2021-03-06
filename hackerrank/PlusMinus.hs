-- https://www.hackerrank.com/challenges/plus-minus/problem
import Control.Exception.Base(assert)
import Text.Printf(printf)

readInputList :: IO([Int])
readInputList = do
  listSize <- readLn :: IO Int
  line <- getLine
  let inputList = map (read :: String -> Int) $ words line
  assert (listSize == length inputList) (return inputList)

getNumbers :: [Int] -> [Int]
getNumbers inputList =
  let positives = filter (>0) inputList
      zeros = filter (==0) inputList
      negatives = filter (<0) inputList
   in map length $ positives:negatives:zeros:[]

ratio :: Int -> Int -> Double
ratio listSize number = fromIntegral(number) / fromIntegral(listSize)

main = do
  inputList <- readInputList
  let numbers = getNumbers(inputList)
  let results = map (ratio (length inputList)) numbers
  mapM_ (printf "%.6f\n") results
