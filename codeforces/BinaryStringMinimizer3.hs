-- http://codeforces.com/problemset/problem/1256/D


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


updateZeroIndex :: Char -> Int -> Int
updateZeroIndex char zeroIndex
  | char == zero = zeroIndex + 1
  | otherwise = zeroIndex


move2 :: Int -> a -> [a] -> [a]
move2 moves element list = take correctedMoves list ++ [element] ++ drop correctedMoves list
  where correctedMoves = (length list) - moves


solveRecursive :: Int -> Int -> Int -> String -> String -> String
solveRecursive moves zeroIndex index unordered ordered
  | unordered == [] = ordered
  | moves == 0 = ordered ++ unordered
  | shouldMove = solveRecursive updatedMoves nextZeroIndex (index + 1) rest updatedOrderedList
  | otherwise = solveRecursive moves nextZeroIndex (index + 1) rest (ordered ++ [first])
  where allowedMoves = min moves $ index - zeroIndex
        updatedMoves = moves - allowedMoves
        first = head unordered
        rest = tail unordered
        shouldMove = first == zero && length ordered > 0 && last ordered == one
        nextZeroIndex = updateZeroIndex first zeroIndex
        updatedOrderedList = move2 allowedMoves first ordered


solve :: Case -> String
solve (Case moves string) = do
  solveRecursive moves 0 0 string []


main :: IO ()
main = interact $
  unlines . map (id . solve . toCase) . -- Why id? See: https://stackoverflow.com/a/12104586/2420718
  toPairs . tail . lines

