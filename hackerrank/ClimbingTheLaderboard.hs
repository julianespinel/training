-- https://www.hackerrank.com/challenges/climbing-the-leaderboard/problem
-- How to run: stack runhaskell ClimbingTheLaderboard.hs
module Main where

import Control.Exception.Base(assert)
import qualified Data.Set as Set

readScores :: Int -> IO([Int])
readScores size = do
  line <- getLine
  let leaderboard = map (read :: String -> Int) $ words line
  assert (size == length leaderboard) (return leaderboard)

getRanks :: [Int] -> [Int] -> Int -> [Int]
getRanks [] scores index = replicate (length scores) index
getRanks leaderboard [] index = []
getRanks (x:xs) (score:scores) index
  | score < x = getRanks xs (score:scores) (index + 1)
  | score == x = index:getRanks (x:xs) scores index
  | score > x = index:getRanks (x:xs) scores index

main = do
  leaderboardSize <- readLn :: IO Int
  leaderboard <- readScores leaderboardSize
  let leaderboardSet = (Set.toDescList . Set.fromDescList) leaderboard
  aliceGamesSize <- readLn :: IO Int
  aliceGames <- readScores aliceGamesSize
  let aliceGamesDesc = reverse aliceGames
  let aliceRanks = getRanks leaderboardSet aliceGamesDesc 0
  let correctedIndicesRanks = map (+1) $ reverse aliceRanks
  mapM_ print correctedIndicesRanks
