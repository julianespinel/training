-- http://codeforces.com/problemset/problem/1257/C

import           Control.Monad
import qualified Data.Map      as Map
import           Data.Maybe


type Case = [Int]
type History = Map.Map
data Record = Record { latestIndex   :: Int
                     , minDifference :: Maybe Int
                     } deriving (Show)


filterTestCases :: [String] -> [String]
filterTestCases lines =
  map snd -- get the values of the indexed list = [2]
  $ filter (odd . fst) -- filter the list by odd indices = [(1,2)]
  $ zip [0..] lines -- create an indexed list = [(0,1),(1,2),(2,3)]


toCase :: String -> Case
toCase line = map read $ words line :: Case


updateRecord :: Int -> Int -> Record -> Record
updateRecord key index record
  | minDifference record == Nothing = record { latestIndex = index, minDifference = currentDifference }
  | currentDifference < minDifference record = record { latestIndex = index, minDifference = currentDifference }
  | otherwise = record { latestIndex = index }
  where currentDifference = Just $ index - latestIndex record


updateHistory :: Int -> Int -> History Int Record -> History Int Record
updateHistory key index history
  | Map.notMember key history = Map.insert key Record { latestIndex=index, minDifference=Nothing } history
  | Map.member key history = Map.insert key (updateRecord key index (fromJust $ Map.lookup key history)) history


getMin :: [Maybe Int] -> Maybe Int
getMin []   = Nothing
getMin list = foldr1 min list


solveRecursive :: Case -> Int -> History Int Record -> Maybe Int
solveRecursive [] index history
  | Map.size history == 0 = Nothing
  | otherwise = getMin $ filter isJust $ map minDifference $ Map.elems history
solveRecursive (x:xs) index history =
  solveRecursive xs (index + 1) $ updateHistory x index history


solve :: Case -> Int
solve [] = -1
solve (x:xs)
  | isNothing solution = -1
  | otherwise = 1 + (fromJust solution)
  where solution = solveRecursive xs (index + 1) history
        history = Map.singleton x Record {latestIndex=index, minDifference=Nothing}
        index = 0


main :: IO ()
main = interact $ unlines . map (show . solve . toCase) . filterTestCases . tail . lines
