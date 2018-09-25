-- https://www.hackerrank.com/challenges/climbing-the-leaderboard/problem
-- How to run: stack runhaskell ClimbingTheLaderboard.hs
import Control.Exception.Base(assert)
import qualified Data.Set as Set

readScores :: Int -> IO([Int])
readScores size = do
  line <- getLine
  let leaderboard = map (read :: String -> Int) $ words line
  assert (size == length leaderboard) (return leaderboard)

rankScore :: [Int] -> [Int] -> Int -> ([Int], Int)
rankScore initial [] score = (initial ++ [score], length initial)
rankScore initial (x:xs) score
  | score > x = (initial ++ score:x:xs, length initial)
  | score == x = (initial ++ x:xs, length initial)
  | score < x = rankScore (initial ++ [x]) xs score

getRanks :: [Int] -> [Int] -> [Int] -> [Int]
getRanks leaderboard [] ranks  = ranks
getRanks leaderboard (score:scores) ranks =
  let (newLeaderboard, rank) = rankScore [] leaderboard score
   in getRanks newLeaderboard scores (ranks ++ [rank])

main = do
  leaderboardSize <- readLn :: IO Int
  leaderboard <- readScores leaderboardSize
  aliceGamesSize <- readLn :: IO Int
  aliceGames <- readScores aliceGamesSize
  let leaderboardNoDuplicates = (Set.toDescList . Set.fromDescList) leaderboard
  let aliceRanks = getRanks leaderboardNoDuplicates aliceGames []
  let correctedIndicesRanks = map (+1) aliceRanks
  mapM_ print correctedIndicesRanks
