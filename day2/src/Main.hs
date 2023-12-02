module Main where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, choice, many1, readP_to_S, satisfy, sepBy1, string)

main :: IO ()
main = do
  testInput <- readInput "day2/test_input"
  input <- readInput "day2/input"
  putStrLn "First problem:"
  putStrLn $ "\tTest input: " ++ show (firstProblem testInput) ++ " == 8"
  putStrLn $ "\tProblem input: " ++ show (firstProblem input) ++ " == 2149"
  putStrLn "Second problem:"
  putStrLn $ "\tTest input: " ++ show (secondProblem testInput) ++ " == 2286"
  putStrLn $ "\tProblem input: " ++ show (secondProblem input) ++ " == 71274"
  where
    readInput file = map parseInput . lines <$> readFile file
    parseInput = fst . last . readP_to_S parseGame

data Color = Green | Red | Blue deriving (Show, Eq)

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
    [ string "green" >> return Green,
      string "red" >> return Red,
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

secondProblem :: [Game] -> Int
secondProblem = sum . map getPower

getPower :: Game -> Int
getPower game = greenMin * redMin * blueMin
  where
    greenMin = getMin Green game
    redMin = getMin Red game
    blueMin = getMin Blue game

getMin :: Color -> Game -> Int
getMin color = maximum . map snd . filter ((== color) . fst) . concat . sets
