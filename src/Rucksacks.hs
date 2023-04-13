module Rucksacks 
  ( parseRucksacks,
    calculatePriorities,
    calculateGroupsPriorities) where

import Data.List (splitAt, intersect, elemIndex, nub)
import Data.List.Split (chunksOf)
import Control.Monad

type Rucksack = String 

parseRucksacks :: String -> [Rucksack]
parseRucksacks = lines

calculatePriorities :: [Rucksack] -> Maybe Int
calculatePriorities = fmap sum . mapM (calculateIdentifierPriority . findIdentifier . divideCompartments) 

divideCompartments :: String -> (String, String)
divideCompartments x = splitAt (length x `div` 2) x

findIdentifier :: ([Char], [Char]) -> Char
findIdentifier (x, y) = head . intersect x $ y

calculateGroupsPriorities :: [Rucksack] -> Maybe Int
calculateGroupsPriorities = fmap sum . mapM (findGroupIdentifier >=> calculateIdentifierPriority) . chunksOf 3

findGroupIdentifier :: [String] -> Maybe Char
findGroupIdentifier (x:xs) = case nub . foldl intersect x $ xs of
                               [identifier]-> Just identifier
                               _ -> error "more than one identifier found on group"

calculateIdentifierPriority :: Char -> Maybe Int
calculateIdentifierPriority x = case fmap (+ 1) . elemIndex x $ (['a' .. 'z'] ++ ['A' .. 'Z']) of
                                  Just priority -> Just priority
                                  Nothing -> error "group identifier in not a valid letter"
