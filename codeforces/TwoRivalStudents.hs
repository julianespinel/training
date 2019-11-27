-- http://codeforces.com/problemset/problem/1257/A

data Case = Case Int Int Int Int


toCase :: String -> Case
toCase inputLine = do
  let ints = map read $ words inputLine :: [Int]
  Case (ints !! 0) (ints !! 1) (ints !! 2) (ints !! 3)


solve :: Case -> Int
solve (Case n x a b)
  | x == 0 = abs (a - b)
  | first == 1 && last == n = abs (a - b)
  | x >= 2 && first > 1 && last < n = solve $ Case n (x - 2) (first - 1) (last + 1)
  | first > 1 = solve $ Case n (x - 1) (first - 1) last
  | last < n = solve $ Case n (x - 1) first (last + 1)
  | otherwise = abs (a - b)
  where first = min a b
        last  = max a b


main :: IO ()
main = interact $ unlines .                     -- Join elements with \n. Ex: unlines ["1", "2", "3"] -> "1\n2\n3\n"
                  map (show . solve . toCase) . -- For each element do: 1) toCase, 2) solve, 3) show
                  tail .                        -- Drop the first element (we dont need it)
                  lines                         -- Read lines from stdin
