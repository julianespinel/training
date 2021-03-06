-- https://www.hackerrank.com/challenges/diagonal-difference/problem
import Control.Monad(replicateM)
import Data.List.Split(splitOn)

getIntSquareMatrix :: Int -> IO([[Int]])
getIntSquareMatrix rows = do
  lines <- replicateM rows getLine
  let intMatrix = (map . map) read $ map words lines
  return intMatrix

getDiagonals :: [[Int]] -> ([Int], [Int])
getDiagonals matrix =
  let size = length(matrix) - 1
      indices = [0..size]
      antiDiagonal = zipWith (!!) matrix indices
      mainDiagonal = zipWith (!!) matrix (reverse indices)
   in (antiDiagonal, mainDiagonal)

absDiagonalDifference :: [Int] -> [Int] -> Int
absDiagonalDifference diagonalOne diagonalTwo =
  let oneSum = sum diagonalOne
      twoSum = sum diagonalTwo
   in abs (oneSum - twoSum)

main = do
  number <- readLn :: IO Int
  matrix <- getIntSquareMatrix number
  let (antiDiagonal, mainDiagonal) = getDiagonals(matrix)
  let result = absDiagonalDifference antiDiagonal mainDiagonal
  print result
