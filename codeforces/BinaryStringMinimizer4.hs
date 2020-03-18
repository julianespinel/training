-- http://codeforces.com/problemset/problem/1256/D

import           Data.List
import           Data.Maybe


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


correctIndex :: Maybe Int -> Int
correctIndex maybe
  | isNothing maybe = 0
  | otherwise       = fromJust maybe + 1


calculateIndicesAfterMoves :: [Int] -> Int -> Maybe Int -> [Int]
calculateIndicesAfterMoves [] moves lastZeroIndex = []
calculateIndicesAfterMoves initialZeros 0 _ = initialZeros
calculateIndicesAfterMoves (initialZero:initialZeros) moves lastZeroIndex =
  newIndex : calculateIndicesAfterMoves initialZeros updatedMoves (Just newIndex)
  where correctedIndex = correctIndex lastZeroIndex
        desiredMoves   = initialZero - correctedIndex
        allowedMoves   = min desiredMoves moves
        updatedMoves   = moves - allowedMoves
        newIndex       = initialZero - allowedMoves


buildFinalList :: [Int] -> Int -> String -> String
buildFinalList [] index string = string
buildFinalList (zero:zeros) index (one:ones)
  | zero == index = zeroChar : buildFinalList zeros (index + 1) ones
  | otherwise     = one : buildFinalList (zero:zeros) (index + 1) ones


solve :: Case -> String
solve (Case moves string) = do
  let initialZeroIndices = elemIndices zeroChar string
  let finalZeroIndices   = calculateIndicesAfterMoves initialZeroIndices moves Nothing
  let ones               = replicate (length string) oneChar
  buildFinalList finalZeroIndices 0 ones


main :: IO ()
main = interact $
  unlines . map (id . solve . toCase) . -- Why id? See: https://stackoverflow.com/a/12104586/2420718
  toPairs . tail . lines

