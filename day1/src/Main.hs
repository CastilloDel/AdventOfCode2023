module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (stripPrefix)
import Debug.Trace (trace)

main :: IO ()
main = do
  testInput <- lines <$> readFile "day1/test_input"
  testInput2 <- lines <$> readFile "day1/test_input2"
  input <- lines <$> readFile "day1/input"
  putStrLn "First problem:"
  putStrLn $ "\tTest input: " ++ show (firstProblem testInput) ++ " == 142"
  putStrLn $ "\tProblem input: " ++ show (firstProblem input) ++ " == 54304"
  putStrLn "Second problem:"
  putStrLn $ "\tTest input: " ++ show (secondProblem testInput2) ++ " == 281"
  putStrLn $ "\tProblem input: " ++ show (secondProblem input) ++ " == 54418"

firstProblem :: [String] -> Int
firstProblem = sum . map (joinFirstAndLast . map digitToInt . filter isDigit)

joinFirstAndLast :: [Int] -> Int
joinFirstAndLast list = (* 10) (head list) + last list

secondProblem :: [String] -> Int
secondProblem = sum . map (joinFirstAndLast . readNumbers)

readNumbers :: String -> [Int]
readNumbers "" = []
readNumbers s = case readNumber s of
  (Just n, rest) -> n : readNumbers rest
  (Nothing, rest) -> readNumbers rest

readNumber :: String -> (Maybe Int, String)
readNumber s
  | isDigit $ head s = (Just $ digitToInt $ head s, tail s)
  | Just _ <- stripPrefix "one" s = (Just 1, tail s)
  | Just _ <- stripPrefix "two" s = (Just 2, tail s)
  | Just _ <- stripPrefix "three" s = (Just 3, tail s)
  | Just _ <- stripPrefix "four" s = (Just 4, tail s)
  | Just _ <- stripPrefix "five" s = (Just 5, tail s)
  | Just _ <- stripPrefix "six" s = (Just 6, tail s)
  | Just _ <- stripPrefix "seven" s = (Just 7, tail s)
  | Just _ <- stripPrefix "eight" s = (Just 8, tail s)
  | Just _ <- stripPrefix "nine" s = (Just 9, tail s)
  | otherwise = (Nothing, tail s)
