module Main where

import Data.Char (digitToInt, isDigit)

main :: IO ()
main = do
  testInput <- lines <$> readFile "day1/test_input"
  input <- lines <$> readFile "day1/input"
  print $ "Test input: " ++ show (firstProblem testInput) ++ " == 142"
  print $ "Problem input: " ++ show (firstProblem input) ++ " == 54304"

firstProblem :: [String] -> Int
firstProblem = sum . map (addFirstAndLast . map digitToInt . filter isDigit)

addFirstAndLast :: [Int] -> Int
addFirstAndLast list = (* 10) (head list) + last list
