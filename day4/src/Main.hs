{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (isDigit)
import Data.IntMap (IntMap, insertWith, (!))
import qualified Data.IntMap as IntMap (elems, fromList)
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
  putStrLn "Second problem:"
  putStrLn $ "\tTest input: " ++ show (secondProblem testInput) ++ " == 30"
  putStrLn $ "\tProblem input: " ++ show (secondProblem input) ++ " == 5833065"
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
getCardWorth = calculatePoints . getNumberOfWinningNumbers
  where
    calculatePoints 0 = 0
    calculatePoints correctNumbers = 2 ^ (correctNumbers - 1)

getNumberOfWinningNumbers :: Card -> Int
getNumberOfWinningNumbers (Card _ winningNumbers playingNumbers) =
  length $ filter (`IntSet.member` winningNumbers) playingNumbers

secondProblem :: [Card] -> Int
secondProblem cards = sum $ IntMap.elems $ foldl accumulateCardNumbers baseIntMap cards
  where
    baseIntMap = IntMap.fromList $ map (,1) (take (length cards) $ iterate (+ 1) 1)

accumulateCardNumbers :: IntMap Int -> Card -> IntMap Int
accumulateCardNumbers quantities card = foldl addNewCards quantities affectedCardNumbers
  where
    addNewCards quantities n = insertWith (+) n numberOfExtraCards quantities
    numberOfExtraCards = quantities ! number card
    affectedCardNumbers = take score $ iterate (+ 1) (1 + number card)
    score = getNumberOfWinningNumbers card
