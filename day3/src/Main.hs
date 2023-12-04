{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.List (nub)
import Data.Matrix (Matrix, ncols, nrows, safeGet, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (catMaybes, mapMaybe)
import Debug.Trace (trace)

main :: IO ()
main = do
  testInput <- Matrix.fromLists . lines <$> readFile "day3/test_input"
  input <- Matrix.fromLists . lines <$> readFile "day3/input"
  putStrLn "First problem:"
  putStrLn $ "\tTest input: " ++ show (firstProblem testInput) ++ " == 4361"
  putStrLn $ "\tProblem input: " ++ show (firstProblem input) ++ " == 531561"
  putStrLn "Second problem:"
  putStrLn $ "\tTest input: " ++ show (secondProblem testInput) ++ " == 4361"
  putStrLn $ "\tProblem input: " ++ show (secondProblem input) ++ " == 531561"

firstProblem :: Matrix Char -> Int
firstProblem a = sum $ map snd $ nub numbersWithPosition
  where
    numbersWithPosition = map (`parseNumber` a) $ filter (`checkCloseToSymbol` a) numberIndexes
    numberIndexes = filter (\pos -> isDigit (a ! pos)) indexes
    indexes = concatMap (\i -> map (i,) [1 .. (ncols a)]) [1 .. nrows a]

checkCloseToSymbol :: (Int, Int) -> Matrix Char -> Bool
checkCloseToSymbol pos a = any (`checkPosIsSymbol` a) possibleIndexes
  where
    possibleIndexes = map (\offset -> bimap (fst offset +) (snd offset +) pos) offsets
    offsets = concatMap (filter (/= (0, 0)) . zip [-1, 0, 1] . replicate 3) [-1, 0, 1]

checkPosIsSymbol :: (Int, Int) -> Matrix Char -> Bool
checkPosIsSymbol (row, col) = (== Just True) . fmap (\a -> not (isDigit a || a == '.')) . safeGet row col

parseNumber :: (Int, Int) -> Matrix Char -> ((Int, Int), Int)
parseNumber (row, col) a = (pos, number)
  where
    number = read $ earlierNumbers ++ (a ! (row, col)) : laterNumbers
    pos = head $ map snd earlierNumbersWithPos ++ [(row, col)]
    laterNumbers = catMaybes . takeWhile (/= Nothing) $ map (\col -> safeGetNumber row col a) $ iterate (+ 1) (col + 1)
    earlierNumbers = map fst earlierNumbersWithPos
    earlierNumbersWithPos = reverse $ takeJusts $ map safeGetNumberWithPos $ iterate (+ (-1)) (col - 1)
    takeJusts = catMaybes . takeWhile (/= Nothing)
    safeGetNumberWithPos col = (,(row, col)) <$> safeGetNumber row col a
    safeGetNumber row col a = case safeGet row col a of
      Just n -> if isDigit n then Just n else Nothing
      Nothing -> Nothing

secondProblem :: Matrix Char -> Int
secondProblem a = sum $ map product gearNumbers
  where
    gearNumbers = filter ((== 2) . length) $ map (map snd . nub) possibleGearNumbers
    possibleGearNumbers = map (map (`parseNumber` a) . (`getSurroundingNumberIndexes` a)) possibleGearIndexes
    possibleGearIndexes = filter (\pos -> '*' == (a ! pos)) indexes
    indexes = concatMap (\i -> map (i,) [1 .. (ncols a)]) [1 .. nrows a]

getSurroundingNumberIndexes :: (Int, Int) -> Matrix Char -> [(Int, Int)]
getSurroundingNumberIndexes pos a = filter (\i -> isDigit (a ! i)) possibleIndexes
  where
    possibleIndexes = map (\offset -> bimap (fst offset +) (snd offset +) pos) offsets
    offsets = concatMap (filter (/= (0, 0)) . zip [-1, 0, 1] . replicate 3) [-1, 0, 1]
