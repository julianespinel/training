-- http://codeforces.com/contest/1256/problem/A


data Case = Case { ns :: Int
                 , ones :: Int
                 , nValue :: Int
                 , total :: Int
                 }


data Answer = YES | NO deriving (Show)


toCase :: String -> Case
toCase line = do
  let ints = map read $ words line :: [Int]
  Case { ns = ints !! 0
       , ones = ints !! 1
       , nValue = ints !! 2
       , total = ints !! 3
       }


getUsedNs :: Case -> Int
getUsedNs (Case ns _ nValue total) = do
    let requiredNs = total `div` nValue
    min ns requiredNs


getUsedOnes :: Int -> Int -> Int -> Int
getUsedOnes totalN ones total = do
    let requiredOnes = total - totalN
    min ones requiredOnes


toAnswer :: Bool -> Answer
toAnswer boolean
  | boolean = YES
  | otherwise = NO


solve :: Case -> Answer
solve (Case ns ones nValue total) = do
  toAnswer (totalN + usedOnes == total)
  where usedNs = getUsedNs (Case ns ones nValue total)
        totalN = usedNs * nValue
        usedOnes = getUsedOnes totalN ones total


main :: IO ()
main = interact $ unlines . map (show . solve . toCase) . tail . lines
