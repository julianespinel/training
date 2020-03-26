-- http://codeforces.com/problemset/problem/1256/D

import Data.List


zeroChar = '0'
oneChar = '1'


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


calculateIndicesAfterMoves :: [Int] -> [Int] -> Int -> [Int]
calculateIndicesAfterMoves [] _ _ = []
calculateIndicesAfterMoves initialZeros _ 0 = initialZeros
calculateIndicesAfterMoves (initialZero:initialZeros) (idealIndex:idealIndices) moves =
  newZeroIndex : calculateIndicesAfterMoves initialZeros idealIndices updatedMoves
  where desiredMoves = initialZero - idealIndex
        allowedMoves = min desiredMoves moves
        updatedMoves = moves - allowedMoves
        newZeroIndex = initialZero - allowedMoves


buildFinalList :: [Int] -> Int -> String -> String
buildFinalList [] index ones = ones
buildFinalList (zero:zeros) index (one:ones)
  | zero == index = zeroChar : buildFinalList zeros (index + 1) ones
  | otherwise     = one : buildFinalList (zero:zeros) (index + 1) ones


solve :: Case -> String
solve (Case moves string) = do
  let initialZeroIndices = elemIndices zeroChar string
  let idealIndices      = [0..(length initialZeroIndices)]
  let finalZeroIndices   = calculateIndicesAfterMoves initialZeroIndices idealIndices moves
  let ones               = replicate (length string) oneChar
  buildFinalList finalZeroIndices 0 ones


main :: IO ()
main = interact $
  unlines . map (id . solve . toCase) . -- Why id? See: https://stackoverflow.com/a/12104586/2420718
  toPairs . tail . lines
