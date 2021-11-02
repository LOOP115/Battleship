-- File: Proj2.hs
-- Author:  Jiahao Chen  1118749  jiahchen4@student.unimelb.edu.au
-- COMP30020 - Declarative Programming: Project 1
-- Semester 2, 2021

-- Purpose: 
-- This program aims to simulate a game called Battleship.
-- It is a two-player logical guessing game which involves one player, 
-- the searcher trying to find the locations of three battleships 
-- hidden by the other player, the hider. The searcher keeps guessing
-- until all the hidden ships are found. It is played on a 4×8 grid.

-- Approach:
-- 1. Set first guess and generate all possibile guesses as candidates.
-- 2. Remove all the candidates which are inconsistent with the answer.
-- 3. Find the best guess from the candidates and proceed.

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char(ord)
import Data.List(sort, group, delete, sortBy)

type GameState = [[Location]]   -- Record current candidates.
newtype Location = Location String deriving (Eq)
instance Show Location where show (Location str) = str

-- Locations of first guess.
firstGuess = map Location ["A1", "E4", "H1"]

-- All the combinations of possible guesses.
-- Rule1 promises that guesses are sorted in decending order.
-- It helps to save memory and time when generating combnations.
candidates = [map Location [[a,b],[c,d],[e,f]] | 
              a <- ['A'..'H'], c <- ['A'..'H'], e <- ['A'..'H'], 
              b <- ['1'..'4'], d <- ['1'..'4'], f <- ['1'..'4'],
              rule1 [a,b] [c,d] && rule1 [c,d] [e,f]]


-----------------------------------------------------------------------------
-- Guess --                                                                 
-----------------------------------------------------------------------------
-- Return a pair of an initial guess and a game state.
initialGuess :: ([Location], GameState)
initialGuess = (firstGuess, firstState)
    where
        firstState =  delete firstGuess candidates

-- Remove all the candidates which are inconsistent with the answer. (Hint3)
-- Find the best guess from the candidates and proceed. (Hint6)
-- Return the new guess and new game state.
nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess (guess, state) answer = (guess2, state2)
    where
        -- Save all the consistent candidates.
        potentials = [target | target <- state, feedback target guess == answer]
        guess2 = bestGuess potentials
        state2 = delete guess2 potentials

-- Return the best choice for the next guess.
-- Deriving from hint6, the best guess gives the max number of possible targets,
-- which is the second element of the first tuple in the sorted list.
-- The list consists of tuples of guesses and their numbers of possible targets.
-- Rule2 helps to sort based on number of possible targets in decending order.
bestGuess :: GameState -> [Location]
bestGuess state  = snd (head sortedDistincts)
    where
        distincts = guessDistinct state state
        sortedDistincts = sortBy rule2 distincts

-- Return list of tuples with guesses and their numbers of possible targets.
-- Format: [(Number of possible targets, [Guess1, Guess2, Guess3])]
-- The logic is that the more the number of possible targets, the more chance
-- the true target might be hidden in this guess.
guessDistinct :: GameState ->  GameState -> [(Int, [Location])]
guessDistinct [] _ = []
guessDistinct (x:xs) state = (distinct, x) : guessDistinct xs state
    where 
        state2 = delete x state
        answers = [answer | guess <- state2, let answer = feedback x guess]
        distinct = length (group (sort answers))


-----------------------------------------------------------------------------
-- Location --
-----------------------------------------------------------------------------
-- Return the location according to the string if valid.
-- Char.ord is used to help standardising.
toLocation :: String -> Maybe Location
toLocation loc
    | x > 0 && x < 9 && y > 0 && y < 5 = Just (Location loc)
    | otherwise = Nothing
        where
            x = ord (head loc) - 64 
            y = ord (last loc) - 48

-- Return the string converted from the location.
fromLocation :: Location -> String
fromLocation loc = show loc


-----------------------------------------------------------------------------
-- Feedback --
-----------------------------------------------------------------------------
-- Return the feedback based on the given exact locations and guesses.
-- The answer is a triple of the number of correct locations, 
-- the number of guesses exactly one square away from a ship, 
-- and the number exaxtly two squares away.
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback locs guesses = feedbacks locs guesses (0, 0, 0)

-- Recursively compute the distance between each guess and ship locations.
-- First check if the guess is located in one of ships.
-- Then check if the guess is one space away from one of ships.
-- Finally check if the guess is two spaces away from one of ships.
feedbacks :: [Location] -> [Location] -> (Int, Int, Int) -> (Int, Int, Int)
feedbacks _ [] res = res 
feedbacks locs (x:xs) (i, j, k)
    | exactLoc x locs == 1 = feedbacks locs xs (i+1, j, k)
    | nLoc 2 x locs == 1 = feedbacks locs xs (i, j+1, k)
    | nLoc 3 x locs == 1 = feedbacks locs xs (i, j, k+1)
    | otherwise = feedbacks locs xs (i, j, k)

-- Judge if the guess is exactly located in the ship.
exactLoc :: Location -> [Location] -> Int
exactLoc _ [] = 0
exactLoc loc (x:xs)
    | loc == x = 1
    | otherwise = exactLoc loc xs

-- Judge if the guess is exactly n spaces away from the ship.
-- Char.ord is used to help computing distance.
nLoc :: Int -> Location -> [Location] -> Int
nLoc _ _ [] = 0
nLoc n loc (x:xs)
    | gx > x1 && gx < x2 && gy > y1 && gy < y2 = 1
    | otherwise = nLoc n loc xs
        where
            gx = ord ((fromLocation loc)!!0)
            gy = ord ((fromLocation loc)!!1)
            ex = ord ((fromLocation x)!!0)
            ey = ord ((fromLocation x)!!1)
            x1 = ex - n
            x2 = ex + n
            y1 = ey - n
            y2 = ey + n


-----------------------------------------------------------------------------
-- Helper functions for sorting--
-----------------------------------------------------------------------------
-- Sorting rule for helping generating combinations of candidates.
-- Char.ord is used to help comparing two Chars.
rule1 :: String -> String -> Bool
rule1 [x1,y1] [x2,y2] = (xx2 > xx1) || (xx2 == xx1 && yy2 > yy1)
    where
        xx1 = ord x1
        xx2 = ord x2
        yy1 = ord y1
        yy2 = ord y2

-- Sorting rule for the list of tuples when finding the best guess.
-- The number of possible targets is the first element of tuple.
-- The maximum number of possible targets is wanted.
rule2 :: (Ord a) => (a, b) -> (a, b) -> Ordering
rule2 x y
    | (fst x) > (fst y)   = LT
    | otherwise           = GT
