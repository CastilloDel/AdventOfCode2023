module Main where

import Data.Char (isDigit)
import Data.HashMap (Map, fromList)
import qualified Data.HashMap as HashMap
import Text.ParserCombinators.ReadP (ReadP, char, choice, many1, readP_to_S, satisfy, sepBy1, string)

main :: IO ()
main = do
  testInput <- readInput "day2/test_input"
  input <- readInput "day2/input"
  putStrLn "First problem:"
  putStrLn $ "\tTest input: " ++ show (firstProblem testInput) ++ " == 8"
  putStrLn $ "\tProblem input: " ++ show (firstProblem input) ++ " == 2149"
  where
    readInput file = map parseInput . lines <$> readFile file
    parseInput = fst . last . readP_to_S parseGame

data Color = Green | Red | Blue deriving (Show)

type CubeSet = [(Color, Int)]

data Game = Game
  { number :: Int,
    sets :: [CubeSet]
  }
  deriving (Show)

parseGame :: ReadP Game
parseGame = do
  string "Game "
  number <- many1 $ satisfy isDigit
  char ':'
  sets <- parseCubeSet `sepBy1` char ';'
  return Game {number = read number, sets = sets}

parseCubeSet :: ReadP CubeSet
parseCubeSet = parseCubes `sepBy1` char ','

parseCubes :: ReadP (Color, Int)
parseCubes = do
  char ' '
  number <- read <$> many1 (satisfy isDigit)
  char ' '
  color <- parseColor
  return (color, number)

parseColor :: ReadP Color
parseColor = do
  choice
    [ string "red" >> return Red,
      string "green" >> return Green,
      string "blue" >> return Blue
    ]

firstProblem :: [Game] -> Int
firstProblem = sum . map number . filter checkGame

checkGame :: Game -> Bool
checkGame = all (all checkCubes) . sets

checkCubes :: (Color, Int) -> Bool
checkCubes (Green, n) = n <= 13
checkCubes (Red, n) = n <= 12
checkCubes (Blue, n) = n <= 14
