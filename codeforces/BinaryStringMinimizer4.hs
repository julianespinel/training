-- http://codeforces.com/problemset/problem/1256/D

import           Data.List
import           Data.Maybe


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


correctIndex :: Maybe Int -> Int
correctIndex maybe
  | isNothing maybe = 0
  | otherwise = fromJust maybe + 1


calculateIndicesAfterMoves :: [Int] -> Int -> Maybe Int -> [Int]
calculateIndicesAfterMoves [] moves lastZeroIndex = []
calculateIndicesAfterMoves (x:xs) moves lastZeroIndex
  | moves == 0 = x:xs
  | otherwise = newIndex : calculateIndicesAfterMoves xs updatedMoves (Just newIndex)
  where correctedIndex = correctIndex lastZeroIndex
        desiredMoves = x - correctedIndex
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
  let finalZeroIndices = calculateIndicesAfterMoves initialZeroIndices moves Nothing
  let ones = replicate (length string) one
  buildFinalList finalZeroIndices 0 ones


main :: IO ()
main = interact $
  unlines . map (id . solve . toCase) . -- Why id? See: https://stackoverflow.com/a/12104586/2420718
  toPairs . tail . lines

