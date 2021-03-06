{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = length $ filter (\i -> c1 !! i == c2 !! i) [0..length c1 - 1]

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = map (\color -> length $ filter (==color) c) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum $ map minimum $ transpose [countColors c1, countColors c2]

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = Move guess m ((matches actual guess) - m)
                       where m = exactMatches actual guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess ematch nematch) code = getMove code guess == Move guess ematch nematch

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodesHelper :: [Code] -> Peg -> [Code]
allCodesHelper codes p = map (p:) codes

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = concatMap (allCodesHelper (allCodes (n-1))) colors

-- Exercise 7 -----------------------------------------

solveHelper :: Code -> [Code] -> [Move]
solveHelper _ [] = []
solveHelper secret codes
  | secret == guess = [move]
  | otherwise = move : solveHelper secret (tail codes)
  where
    guess = head codes
    move = getMove secret guess

solve :: Code -> [Move]
solve secret = solveHelper secret (allCodes $ length secret)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
