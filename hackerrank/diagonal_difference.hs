-- https://www.hackerrank.com/challenges/diagonal-difference/problem
import Control.Monad(replicateM)
import Data.List.Split(splitOn)

nestedMap :: (a -> b) -> [[a]] -> [[b]]
nestedMap = map . map

getIntSquareMatrix :: Int -> IO([[Int]])
getIntSquareMatrix side = do
  matrix <- replicateM (side) getLine
  let intMatrix = nestedMap read $ map words matrix
  return intMatrix

getDiagonalElement :: Int -> [Int] -> Int
getDiagonalElement rowIndex row = row !! rowIndex

getDiagonals :: [[Int]] -> ([Int], [Int])
getDiagonals matrix =
  let size = length(matrix) - 1
      indices = [0..size]
      leftRightDiagonal = zipWith getDiagonalElement indices matrix
      rightLeftDiagonal = zipWith getDiagonalElement (reverse indices) matrix
   in (leftRightDiagonal, rightLeftDiagonal)

absDiagonalDifference :: [Int] -> [Int] -> Int
absDiagonalDifference diagonalOne diagonalTwo =
  let oneSum = foldr (+) 0 diagonalOne
      twoSum = foldr (+) 0 diagonalTwo
   in abs (oneSum - twoSum)

main = do
  numberString <- getLine
  let number = read numberString :: Int
  matrix <- getIntSquareMatrix number
  let (rightLeft, leftRight) = getDiagonals(matrix)
  let result = absDiagonalDifference rightLeft leftRight
  print result
