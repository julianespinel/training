-- http://codeforces.com/problemset/problem/1256/D


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


firstZeros :: String -> Int -> [Int]
firstZeros [] index = []
firstZeros (x:xs) index
  | x == '0' = index : firstZeros xs (index + 1)
  | otherwise = firstZeros xs (index + 1)


calculateNewElement :: Int -> Int -> Int
calculateNewElement updatedMoves index
  | updatedMoves > 0 = index
  | otherwise = index - updatedMoves


replaceIndexBy :: Int -> a -> [a] -> [a]
replaceIndexBy index newElement list = do
  let tuple = splitAt (index + 1) list
  init (fst tuple) ++ [newElement] ++ snd tuple


isInOrder :: [Int] -> Bool
isInOrder indices = indices == take (length indices) [0..]


zerosIndicesAfterMoves :: Int -> Int -> [Int] -> [Int]
zerosIndicesAfterMoves moves index indices
  | isInOrder indices = indices
  | moves <= 0 = indices
  | updatedMoves > 0 = zerosIndicesAfterMoves updatedMoves (index + 1) $ replaceIndexBy index newElement indices
  | otherwise = zerosIndicesAfterMoves updatedMoves (index + 1) $ replaceIndexBy index newElement indices
  where element = indices !! index
        requiredMoves = element - index
        updatedMoves = moves - requiredMoves
        newElement = calculateNewElement updatedMoves index


setZeros :: [Int] -> [Int] -> [Int]
setZeros ones [] = ones
setZeros ones (x:xs) =
  setZeros updatedOnes xs
  where updatedOnes = replaceIndexBy x 0 ones


solve :: Case -> String
solve (Case moves string) =
  foldr1 (++) $ map show solutionInts
  where solutionInts = setZeros ones zerosAfterMoves
        zerosAfterMoves = zerosIndicesAfterMoves moves 0 $ firstZeros string 0
        ones = take (length string) $ repeat 1


main :: IO ()
main = interact $
  unlines . map (id . solve . toCase) . -- Why id? See: https://stackoverflow.com/a/12104586/2420718
  toPairs . tail . lines

