-- Problem statement: https://www.hackerrank.com/challenges/climbing-the-leaderboard/problem
-- How to run: stack runhaskell ClimbingTheLeaderboard.hs
-- How to generate docs: stack exec -- haddock --html ClimbingTheLeaderboard.hs --odir=./docs
module Main where

import Control.Exception.Base(assert)
import qualified Data.Set as Set

-- | The 'readScores' function takes a value indicating the amount
-- of words to read from stdin.
--
-- The function turns the words read from stdin into to a list of 'Int's
-- and returns it inside the IO monad.
readScores :: Int -> IO([Int])
readScores size = do
  line <- getLine
  let leaderboard = map (read :: String -> Int) $ words line
  assert (size == length leaderboard) (return leaderboard)

-- | The 'getRanks' fucntion takes three parameters:
--
-- 1. leaderboard: a list of 'Int's in descending order, representing
--   a leaderboard.
--
-- 2. scores: a list of 'Int's in descending order, representing the scores
--   of a single player.
--
-- 3. index: an 'Int' to represent the current position we are evaluating in
--   in the leaderboard.
--
-- This function returns a list of 'Int's. Each element in the result
-- represents the position of the user in the leaderboard per each score
-- given in the second parameter 'scores'.
getRanks :: [Int] -> [Int] -> Int -> [Int]
getRanks [] scores index = replicate (length scores) index
getRanks leaderboard [] index = []
getRanks (x:xs) (score:scores) index
  | score < x = getRanks xs (score:scores) (index + 1)
  | score == x = index:getRanks (x:xs) scores index
  | score > x = index:getRanks (x:xs) scores index

-- | The 'main' function is the entry point of the program
main :: IO()
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
