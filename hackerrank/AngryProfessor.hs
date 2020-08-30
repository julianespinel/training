-- https://www.hackerrank.com/challenges/angry-professor/problem

import           Text.Printf

data Answer = YES | NO deriving (Show)

data Case = Case { totalStudents         :: Int
                 , cancellationThreshold :: Int
                 , arrivals              :: [Int] }

toPairs :: [a] -> [(a, a)]
toPairs []                  = []
toPairs [_]                 = error "list size is odd, should be even" -- Break the program on erroneous input
toPairs (first:second:tail) = (first, second) : toPairs tail

toCase :: (String, String) -> Case
toCase (firstLine, secondLine) = do
  Case { totalStudents = students
       , cancellationThreshold = threshold
       , arrivals = map read $ words secondLine :: [Int] }
  where firstList = map read $ words firstLine :: [Int]
        students = head firstList
        threshold = firstList !! 1

validate :: Case -> Case
validate (Case students threshold arrivals)
  | students == length arrivals = Case students threshold arrivals
  | otherwise = error errorMessage -- Break the program on erroneous input
  where errorMessage = printf "Total students expected to be %s, but is %s" (show students) (show $ length arrivals)

countEarly :: [Int] -> Int
countEarly arrivals = length $ filter (<= 0) arrivals

isClassCancelled :: Case -> Answer
isClassCancelled (Case _ cancellationThreshold arrivals)
  | countEarly arrivals >= cancellationThreshold = NO
  | otherwise = YES

solve :: Case -> Answer
solve = isClassCancelled . validate

main =
  interact $                    -- Reads an input, prints an output
  unlines .                     -- Join elements with \n. Ex: unlines ["YES", "NO", "YES"] -> "YES\nNO\nYES\n"
  map (show . solve . toCase) . -- For each element do: 1) toCase, 2) solve, 3) show
  toPairs .                     -- Each test case has two lines, put tme in a pair: (line 1, line 2)
  tail .                        -- Drop the first element (we dont need it)
  lines                         -- Read lines from stdin
