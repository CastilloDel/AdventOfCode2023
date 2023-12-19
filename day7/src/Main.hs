module Main where

import Data.List (sort)
import Data.Map (fromListWith, toList)
import Data.Ord (Down (Down), comparing)
import Debug.Trace (trace)
import GHC.OldList (sortBy)

main :: IO ()
main = do
  testInput <- readInput <$> readFile "day7/test_input"
  input <- readInput <$> readFile "day7/input"
  putStrLn "First problem:"
  putStrLn $ "\tTest input: " ++ show (firstProblem testInput) ++ " == 6440"
  putStrLn $ "\tProblem input: " ++ show (firstProblem input) ++ " == 1413720"
  where
    readInput = map parsePlay . lines

data Play = Play
  { cards :: [Card],
    bid :: Int
  }
  deriving (Show)

data Hand = Hand
  { handCards :: [Card],
    handType :: HandType
  }
  deriving (Show, Eq)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Joker | Queen | King | Ace deriving (Show, Eq, Ord)

data HandType = HighCard | Pair | DoublePair | ThreeOfAKind | FullHouse | Poker | Repoker deriving (Show, Eq, Ord)

parsePlay :: String -> Play
parsePlay s = Play hand bid
  where
    hand = map parseCard $ take 5 s
    bid = read $ drop 6 s

parseCard :: Char -> Card
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = Ten
parseCard 'J' = Joker
parseCard 'Q' = Queen
parseCard 'K' = King
parseCard 'A' = Ace

firstProblem :: [Play] -> Int
firstProblem = sum . zipWith (*) (iterate (+ 1) 1) . map (bid . fst) . sortBy (\a b -> compareHand (snd a) (snd b)) . map (\a -> (a, getHand a))

getHand :: Play -> Hand
getHand (Play cards _) = Hand cards $ getHandType occurrences
  where
    occurrences = sortBy (comparing Down) (map snd $ toList (fromListWith (+) [(c, 1) | c <- cards]))
    getHandType [5] = Repoker
    getHandType (4 : _) = Poker
    getHandType [3, 2] = FullHouse
    getHandType (3 : _) = ThreeOfAKind
    getHandType (2 : 2 : _) = DoublePair
    getHandType (2 : _) = Pair
    getHandType (1 : _) = HighCard

compareHand :: Hand -> Hand -> Ordering
compareHand (Hand ca ha) (Hand cb hb)
  | ha == hb = compareCards ca cb
  | otherwise = compare ha hb

compareCards :: [Card] -> [Card] -> Ordering
compareCards (a : ra) (b : rb)
  | a == b = compareCards ra rb
  | otherwise = compare a b
