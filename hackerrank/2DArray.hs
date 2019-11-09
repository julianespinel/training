-- https://www.hackerrank.com/challenges/2d-array/problem

import Control.Monad (replicateM, liftM2)
import Data.Maybe (fromJust)

type HourGlass = [Int]


squareSide = 6
hourGlassSide = 3


getIntSquareMatrix :: Int -> IO([[Int]])
getIntSquareMatrix rows = do
  lines <- replicateM rows getLine
  let intMatrix = (map . map) read $ map words lines
  return intMatrix


getAllCoordinates :: Int -> [(Int, Int)]
getAllCoordinates side = liftM2 (,) [0..side] [0..side]


buildHourGlass :: [[Int]] -> (Int, Int) -> Maybe HourGlass
buildHourGlass matrix (row, col) = do
  let first = (take hourGlassSide . drop col) $ matrix !! row
      mid = (take 1 . drop (col + 1)) $ matrix !! (row + 1)
      last = (take hourGlassSide . drop col) $ matrix !! (row + 2)
  Just $ first ++ mid ++ last


getHourGlass :: [[Int]] -> (Int, Int) -> Maybe HourGlass
getHourGlass matrix (row, col)
  | (row + hourGlassSide - 1) >= squareSide = Nothing
  | (col + hourGlassSide - 1) >= squareSide = Nothing
  | otherwise = buildHourGlass matrix (row, col)


getHourGlasses :: [[Int]] -> [(Int, Int)] -> [Maybe HourGlass]
getHourGlasses matrix coordinates = map (getHourGlass matrix) coordinates


sumHourGlasses :: [Maybe HourGlass] -> [Maybe Int]
sumHourGlasses hourGlasses = (map . fmap . foldr1) (+) hourGlasses


main = do
  matrix <- getIntSquareMatrix squareSide
  let coordinates = getAllCoordinates squareSide
      hourGlasses = getHourGlasses matrix coordinates
      sums = sumHourGlasses hourGlasses
  print $ fromJust(maximum sums)

