module Main
  ( main,
    dayOnePartOne,
    dayOnePartTwo,
    dayTwoPartOne,
    dayTwoPartTwo,
    dayThreePartOne
  ) where

import Elves (parseElvesNotes, maxCalories, topThreeCalories)
import RockPaperScisors (scoreMoves, scoreResults, parseMovesStrategyGuide, parseResultStrategyGuide)
import Rucksacks (parseRucksacks, calculatePriorities, calculateGroupsPriorities)

main :: IO Int
main = dayThreePartOne 

elvesNotesPath :: FilePath
elvesNotesPath = "./elves_notes.txt"

dayOnePartOne :: IO Integer
dayOnePartOne =
  do
    fileContent <- readFile elvesNotesPath
    case parseElvesNotes fileContent of
      Just result -> return . maxCalories $ result
      Nothing -> error "error parsing file"

dayOnePartTwo :: IO Integer
dayOnePartTwo =
  do
    fileContent <- readFile elvesNotesPath
    case parseElvesNotes fileContent of
      Just result -> return . topThreeCalories $ result
      Nothing -> error "error parsing file"

strategyGuidePath :: FilePath
strategyGuidePath = "./strategy_guide.txt"

dayTwoPartOne :: IO Integer
dayTwoPartOne =
  do
    fileContent <- readFile strategyGuidePath
    case parseMovesStrategyGuide fileContent of
      Just result -> return . scoreMoves $ result
      Nothing -> error "error parsing file"

dayTwoPartTwo :: IO Integer
dayTwoPartTwo =
  do
    fileContent <- readFile strategyGuidePath
    case parseResultStrategyGuide fileContent of
      Just result -> return . scoreResults $ result
      Nothing -> error "error parsing file"

rucksacksPath :: FilePath
rucksacksPath = "./rucksacks.txt"

dayThreePartOne :: IO Int
dayThreePartOne =
  do
    fileContent <- readFile rucksacksPath 
    case calculatePriorities . parseRucksacks $ fileContent of
      Just priorities -> return priorities
      Nothing -> error "error parsing file"

dayThreePartTwo :: IO Int
dayThreePartTwo =
  do
    fileContent <- readFile rucksacksPath 
    case calculateGroupsPriorities . parseRucksacks $ fileContent of
      Just priorities -> return priorities
      Nothing -> error "error parsing file"
