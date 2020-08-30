-- https://www.hackerrank.com/challenges/angry-professor/problem

import           Text.Printf

data Case = Case { totalStudents         :: Int
                 , cancellationThreshold :: Int
                 , arrivalTimes          :: [Int] }

data Answer = YES | NO deriving (Show)

toPairs :: [a] -> [(a, a)]
toPairs []                  = []
toPairs [_]                 = error "list size is odd" -- Break the program on erroneous input
toPairs (first:second:tail) = (first, second) : toPairs tail

toCase :: (String, String) -> Case
toCase (firstLine, secondLine) = do
  Case { totalStudents = students
       , cancellationThreshold = threshold
       , arrivalTimes = times }
  where firstList = map read $ words firstLine :: [Int]
        students = head firstList
        threshold = firstList !! 1
        times = map read $ words secondLine :: [Int]

isValid :: Case -> Case
isValid aCase
  | students == length times = aCase
  | otherwise = error errorMessage -- Break the program on erroneous input
  where students = totalStudents aCase
        times = arrivalTimes aCase
        errorMessage = printf "Total students expected to be %s, but is %s" (show students) (show $ length times)

countEarly :: [Int] -> Int
countEarly arrivalTimes = length $ filter (<= 0) arrivalTimes

isClassCancelled :: Case -> Answer
isClassCancelled (Case _ cancellationThreshold arrivalTimes)
  | countEarly arrivalTimes >= cancellationThreshold = NO
  | otherwise = YES

solve :: Case -> Answer
solve = isClassCancelled . isValid

main =
  interact $                    -- Reads an input, prints an output
  unlines .                     -- Join elements with \n. Ex: unlines ["YES", "NO", "YES"] -> "YES\nNO\nYES\n"
  map (show . solve . toCase) . -- For each element do: 1) toCase, 2) solve, 3) show
  toPairs .                     -- Each test case has two lines, put tme in a pair: (line 1, line 2)
  tail .                        -- Drop the first element (we dont need it)
  lines                         -- Read lines from stdin
