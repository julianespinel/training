-- https://www.hackerrank.com/challenges/diagonal-difference/problem
import Control.Monad(replicateM)
import Data.List.Split(splitOn)

nestedMap :: (a -> b) -> [[a]] -> [[b]]
nestedMap = map . map

getIntSquareMatrix :: Int -> IO([[Int]])
getIntSquareMatrix rows = do
  matrix <- replicateM rows getLine
  let intMatrix = nestedMap read $ map words matrix
  return intMatrix

getListElement :: Int -> [Int] -> Int
getListElement rowIndex row = row !! rowIndex

getDiagonals :: [[Int]] -> ([Int], [Int])
getDiagonals matrix =
  let size = length(matrix) - 1
      indices = [0..size]
      antiDiagonal = zipWith getListElement indices matrix
      mainDiagonal = zipWith getListElement (reverse indices) matrix
   in (antiDiagonal, mainDiagonal)

absDiagonalDifference :: [Int] -> [Int] -> Int
absDiagonalDifference diagonalOne diagonalTwo =
  let oneSum = foldr (+) 0 diagonalOne
      twoSum = foldr (+) 0 diagonalTwo
   in abs (oneSum - twoSum)

main = do
  number <- readLn :: IO Int
  matrix <- getIntSquareMatrix number
  let (antiDiagonal, mainDiagonal) = getDiagonals(matrix)
  let result = absDiagonalDifference antiDiagonal mainDiagonal
  print result
