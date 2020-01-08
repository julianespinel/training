-- http://codeforces.com/problemset/problem/1256/D

import           Data.List

zero = '0'
one = '1'


data Case = Case { moves  :: Int
                 , string :: String }


getMoves :: String -> Int
getMoves string = read $ last $ words string :: Int


toCase :: (String, String) -> Case
toCase (one, two) = Case { moves = getMoves one, string = two }


toPairs :: [a] -> [(a,a)]
toPairs []                  = []
toPairs [_]                 = error "list size is odd"
toPairs (first:second:tail) = (first, second) : toPairs tail


calculateIndicesAfterMoves :: [Int] -> Int -> [Int] -> [Int]
calculateIndicesAfterMoves [] moves finalIndices = finalIndices
calculateIndicesAfterMoves (x:xs) moves finalIndices
  | moves == 0 = finalIndices ++ (x:xs)
  | otherwise = calculateIndicesAfterMoves xs updatedMoves (finalIndices ++ [newIndex])
  where lastZeroIndex = if null finalIndices then 0 else (last finalIndices) + 1
        desiredMoves = x - lastZeroIndex
        allowedMoves = min desiredMoves moves
        updatedMoves = moves - allowedMoves
        newIndex = x - allowedMoves


buildFinalList :: [Int] -> Int -> String -> String
buildFinalList [] index string = string
buildFinalList (x:xs) index (y:ys)
  | x == index = zero : buildFinalList xs (index + 1) ys
  | otherwise = y : buildFinalList (x:xs) (index + 1) ys


solve :: Case -> String
solve (Case moves string) = do
  let initialZeroIndices = elemIndices zero string
  let finalZeroIndices = calculateIndicesAfterMoves initialZeroIndices moves []
  let ones = replicate (length string) one
  buildFinalList finalZeroIndices 0 ones


main :: IO ()
main = interact $
  unlines . map (id . solve . toCase) . -- Why id? See: https://stackoverflow.com/a/12104586/2420718
  toPairs . tail . lines

