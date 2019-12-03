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


isSolved :: String -> Bool
isSolved []                  = True
isSolved [_]                 = True
isSolved (first:second:tail) = first <= second && isSolved (second:tail)


order :: String -> String
order [] = []
order (first:second:tail)
  | first > second = second : first : tail
  | otherwise = first : (order $ second : tail)
order string = string


solve :: Case -> String
solve (Case moves string)
  | isSolved string = string
  | moves > 0 = solve Case { moves = moves - 1, string = order string }
  | otherwise = string


main :: IO ()
main = interact $
  unlines . map (show . solve . toCase) .
  toPairs . tail . lines
