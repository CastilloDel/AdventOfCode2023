module Main where

import Data.Char (isSpace)
import Debug.Trace (trace)
import Text.ParserCombinators.ReadP (ReadP, char, many1, readP_to_S, readS_to_P, satisfy, sepBy1, skipSpaces, string)

main :: IO ()
main = do
  testInput <- readInput <$> readFile "day6/test_input"
  input <- readInput <$> readFile "day6/input"
  putStrLn "First problem:"
  putStrLn $ "\tTest input: " ++ show (firstProblem testInput) ++ " == 288"
  putStrLn $ "\tProblem input: " ++ show (firstProblem input) ++ " == 1413720"
  where
    readInput = fst . last . readP_to_S parseProblemData

data ProblemData = ProblemData
  { times :: [Int],
    records :: [Int]
  }
  deriving (Show)

parseProblemData :: ReadP ProblemData
parseProblemData = do
  string "Time:"
  skipSpaces
  times <- sepBy1 (readS_to_P reads) $ many1 $ satisfy isSpace
  skipSpaces
  string "Distance:"
  skipSpaces
  ProblemData times <$> sepBy1 (readS_to_P reads) (many1 $ satisfy isSpace)

firstProblem :: ProblemData -> Int
firstProblem (ProblemData times records) = product $ map length $ zipWith getFeasibleStrategies times records

getFeasibleStrategies :: Int -> Int -> [Int]
getFeasibleStrategies totalTime record = filter ((> record) . getDistance totalTime) [0 .. totalTime]

getDistance :: Int -> Int -> Int
getDistance totalTime timePressing = (totalTime - timePressing) * timePressing
