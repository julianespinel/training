-- https://www.hackerrank.com/challenges/eval-ex/problem

module Main where

import Text.Printf

toInt :: String -> Double
toInt str = read str :: Double

factorial :: Int -> Int
factorial x = foldl (*) 1 [1..x]

calculateExpSeries :: Double -> Int -> Double
calculateExpSeries x index
    | index == 0 = 1
    | index == 1 = x
    | otherwise  = (x ^ index)/doubleFactorial
    where doubleFactorial = fromIntegral (factorial index) :: Double

solve :: Double -> Double
solve x = sum $ map (calculateExpSeries x) [0..9]

format :: Double -> String
format = printf "%.4f"

main :: IO()
main = interact $
  unlines . map (format . solve . toInt) .
  tail . lines

