-- https://www.hackerrank.com/challenges/grading/problem

-- Constants
threshold = 3

lowerLimit = 38

toInt :: String -> Int
toInt string = read string :: Int

calculateNextMultipleOfFive :: Int -> Int
calculateNextMultipleOfFive grade = grade + 5 - (grade `mod` 5)

gradeStudent :: Int -> Int
gradeStudent grade
  | grade < lowerLimit = grade
  | otherwise = if difference < threshold then nextMultiple else grade
  where
    nextMultiple = calculateNextMultipleOfFive grade
    difference = nextMultiple - grade

readGrades :: String -> [String]
readGrades = tail . lines

main :: IO ()
main =
  interact $ unlines . map (show . gradeStudent . toInt) . readGrades
