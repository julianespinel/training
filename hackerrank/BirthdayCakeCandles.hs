-- https://www.hackerrank.com/challenges/birthday-cake-candles/problem

toInts :: String -> [Int]
toInts line = map read $ words line :: [Int]

solve :: [Int] -> Int
solve ints = length $ filter (== maxValue) ints
  where maxValue = maximum ints

main :: IO()
main = interact $ unlines . map (show . solve . toInts) . tail . lines
