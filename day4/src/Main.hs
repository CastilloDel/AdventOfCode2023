module Main where

import Data.Char (isDigit)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (fromList, member)
import Text.ParserCombinators.ReadP (ReadP, char, get, many1, readP_to_S, readS_to_P, satisfy, sepBy1, skipSpaces, string)

main :: IO ()
main = do
  testInput <- readInput <$> readFile "day4/test_input"
  input <- readInput <$> readFile "day4/input"
  putStrLn "First problem:"
  putStrLn $ "\tTest input: " ++ show (firstProblem testInput) ++ " == 13"
  putStrLn $ "\tProblem input: " ++ show (firstProblem input) ++ " == 20667"
  where
    readInput = map parseInput . lines
    parseInput = fst . last . readP_to_S parseCard

data Card = Card
  { number :: Int,
    winningNumbers :: IntSet,
    playingNumbers :: [Int]
  }
  deriving (Show)

parseCard :: ReadP Card
parseCard = do
  string "Card "
  skipSpaces
  number <- read <$> many1 (satisfy isDigit)
  string ": "
  winningNumbers <- parseWinningNumbers
  string " | "
  Card number winningNumbers <$> parsePlayingNumbers

parsePlayingNumbers :: ReadP [Int]
parsePlayingNumbers = sepBy1 parseInt (char ' ')

parseWinningNumbers :: ReadP IntSet
parseWinningNumbers = IntSet.fromList <$> sepBy1 parseInt (char ' ')

parseInt :: ReadP Int
parseInt = readS_to_P reads

firstProblem :: [Card] -> Int
firstProblem = sum . map getCardWorth

getCardWorth :: Card -> Int
getCardWorth (Card _ winningNumbers playingNumbers) =
  calculatePoints $ length $ filter (`IntSet.member` winningNumbers) playingNumbers
  where
    calculatePoints 0 = 0
    calculatePoints correctNumbers = 2 ^ (correctNumbers - 1)
