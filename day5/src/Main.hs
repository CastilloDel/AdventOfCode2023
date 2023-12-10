module Main where

import Data.Char (isDigit)
import Data.List (find)
import Text.ParserCombinators.ReadP (ReadP, char, many1, readP_to_S, readS_to_P, satisfy, sepBy1, string)

main :: IO ()
main = do
  testInput <- readInput <$> readFile "day5/test_input"
  input <- readInput <$> readFile "day5/input"
  putStrLn "First problem:"
  putStrLn $ "\tTest input: " ++ show (firstProblem testInput) ++ " == 35"
  putStrLn $ "\tProblem input: " ++ show (firstProblem input) ++ " == 510109797"
  putStrLn "Second problem:"
  putStrLn $ "\tTest input: " ++ show (secondProblem testInput) ++ " == 46"
  putStrLn $ "\tProblem input: " ++ show (secondProblem input) ++ " == 9622622"
  where
    readInput = fst . last . readP_to_S parseProblemData

data ProblemData = ProblemData
  { seeds :: [Int],
    mappings :: [Mapping]
  }
  deriving (Show)

type Mapping = [(Int, Int, Int)]

parseProblemData :: ReadP ProblemData
parseProblemData = do
  string "seeds: "
  seeds <- sepBy1 (readS_to_P reads) $ char ' '
  string "\n\n"
  ProblemData seeds <$> sepBy1 parseMapping (string "\n\n")

parseMapping :: ReadP [(Int, Int, Int)]
parseMapping = do
  many1 $ satisfy (/= '\n')
  char '\n'
  sepBy1 parseMappingLine (string "\n")

parseMappingLine :: ReadP (Int, Int, Int)
parseMappingLine = do
  n1 <- readS_to_P reads
  char ' '
  n2 <- readS_to_P reads
  char ' '
  n3 <- readS_to_P reads
  return (n1, n2, n3)

firstProblem :: ProblemData -> Int
firstProblem (ProblemData seeds mappings) = minimum $ map (`applyMappings` mappings) seeds

applyMappings :: Int -> [Mapping] -> Int
applyMappings = foldl applyMapping

applyMapping :: Int -> Mapping -> Int
applyMapping start mapping
  | 1 == length possibleMatch = calculateDest (head possibleMatch) start
  | otherwise = start
  where
    possibleMatch = filter (\(_, source, len) -> start >= source && start <= source + len) mapping
    calculateDest (dest, source, _) n = n - source + dest

secondProblem :: ProblemData -> Int
secondProblem (ProblemData seeds mappings) = minimum $ map fst $ applyMappings' seedRanges mappings
  where
    seedRanges = getSeedRanges seeds
    getSeedRanges (a : b : c) = (a, b) : getSeedRanges c
    getSeedRanges [] = []

applyMappings' :: [(Int, Int)] -> [Mapping] -> [(Int, Int)]
applyMappings' = foldl applyMapping'

applyMapping' :: [(Int, Int)] -> Mapping -> [(Int, Int)]
applyMapping' ranges mapping = concatMap (`applyMappingToRange` mapping) ranges

applyMappingToRange :: (Int, Int) -> Mapping -> [(Int, Int)]
applyMappingToRange (start, len) mapping = maybe [(start, len)] exploreMatch firstMatch
  where
    firstMatch = find (\(dest, source, matchLen) -> start + len > source && start < source + matchLen) mapping
    exploreMatch (dest, source, matchLen)
      | start < source && start + len <= source + matchLen = (dest, len - (source - start)) : applyMappingToRange (start, source - start) mapping
      | start >= source && start + len > source + matchLen = (dest + (start - source), matchLen - (start - source)) : applyMappingToRange (source + matchLen + 1, matchLen - (start - source)) mapping
      | start < source && start + len > source + matchLen = (dest, matchLen) : applyMappingToRange (start, source - start) mapping ++ applyMappingToRange (source + matchLen + 1, len - matchLen - (source - start)) mapping
      | otherwise = [(dest + (start - source), len)]
