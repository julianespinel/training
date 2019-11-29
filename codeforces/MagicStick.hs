-- http://codeforces.com/problemset/problem/1257/B

import qualified Data.Map as Map


data Case = Case Int Int
type History = Map.Map
data Response = YES | NO deriving (Show)


toCase :: String -> Case
toCase line = do
  let numbers = map read $ words line :: [Int]
  Case (head numbers) (last numbers)


solveWithHistory :: Case -> History Int Int -> Response
solveWithHistory (Case source target) history
  | source >= target = YES -- We can always reach a lower number by doing source - 1
  | source == 2 && target == 3 = YES -- This is an aceptional case because (3 * 2) / 2 = 3
  | source > 3 = YES -- If source > 3 source can increase (slowly) until reaching the target
  | otherwise = NO
  where updatedHistory = Map.insert source source history


solve :: Case -> Response
solve (Case source target) = solveWithHistory (Case source target) Map.empty


main :: IO ()
main = interact $ unlines . map (show . solve . toCase) . tail . lines
