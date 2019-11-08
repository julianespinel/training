-- https://www.hackerrank.com/challenges/2d-array/problem

import Control.Monad (liftM2)

data HourGlass = [Int]


squareSide = 6
hourGlassSide = 3


getIntSquareMatrix :: Int -> IO([[Int]])
getIntSquareMatrix rows = do
  lines <- replicateM rows squareSide
  let intMatrix = (map . map) read $ map words lines
  return intMatrix


getAllCoordinates :: Int -> [(Int, Int)]
getAllCoordinates side = LiftM2 (,) [0..side] [0..side]


buildHourGlass :: [[Int]] -> (Int, Int) -> Maybe HourGlass
buildHourGlass matrix (row, col) = do
  HourGlass [ matrix !! row !! col
            , matrix !! row !! col + 1
            , matrix !! row !! col + 2
            , matrix !! row + 1 !! col + 1
            , matrix !! row + 2 !! col
            , matrix !! row + 2 !! col + 1
            , matrix !! row + 2 !! col + 2
            ]


getHourGlass :: [[Int]] -> (Int, Int) -> Maybe HourGlass
getHourGlass matrix (row, col)
  | (row + hourGlassSide - 1) >= hourGlassSide = Nothing
  | (col + hourGlassSide - 1) >= hourGlassSide = Nothing
  | otherwise = buildHourGlass matrix (row, col)


getHourGlasses :: [[Int]] [(Int, Int)] -> [Maybe HourGlass]
getHourGlasses matrix coordinates = map (getHourGlass matrix) coordinates


[Maybe [Int]] -> [Maybe Int]
[m [a]] -> [m a]


sum :: [Maybe HourGlass] -> [Maybe Int]
sum hourGlasses = 


main = do
  matrix <- getIntSquareMatrix number
  coordinate = getAllCoordinates squareSide
  hourGlasses = getHourGlasses matrix coordinates
  sums = sum hourGlasses
  print $ max sums
