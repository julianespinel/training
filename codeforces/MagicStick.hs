-- http://codeforces.com/problemset/problem/1257/B

import qualified Data.Map as Map


data Case = Case Int Int
type History = Map.Map
data Response = YES | NO deriving (Show)


toCase :: String -> Case
toCase line = do
  let numbers = map read $ words line :: [Int]
  Case (head numbers) (last numbers)


firstOperation :: Int -> Int
firstOperation number = (3 * number) `div` 2


secondOperation :: Int -> Int
secondOperation number = number - 1


solveWithHistory :: Case -> History Int Int -> Response
solveWithHistory (Case source target) history
  | source == target = YES
  | Map.member source history = NO -- A loop has been detected
  | source < target && even source = solveWithHistory (Case (firstOperation source) target) updatedHistory
  | source > 1 = solveWithHistory (Case (secondOperation source) target) updatedHistory
  | otherwise = NO
  where updatedHistory = Map.insert source source history


solve :: Case -> Response
solve (Case source target) = solveWithHistory (Case source target) Map.empty


main :: IO ()
main = interact $ unlines . map (show . solve . toCase) . tail . lines
