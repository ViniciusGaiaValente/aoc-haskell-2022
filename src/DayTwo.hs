module DayTwo
  ( partOne,
    partTwo,
  )
where

import Input (parseMovesStrategyGuide, parseResultStrategyGuide)
import RockPaperScisors (scoreMoves, scoreResults)

strategyGuidePath :: FilePath
strategyGuidePath = "./strategy_guide.txt"

partOne :: IO Integer
partOne =
  do
    fileContent <- readFile strategyGuidePath
    case parseMovesStrategyGuide fileContent of
      Just result -> return . scoreMoves $ result
      Nothing -> error "error parsing file"

partTwo :: IO Integer
partTwo =
  do
    fileContent <- readFile strategyGuidePath
    case parseResultStrategyGuide fileContent of
      Just result -> return . scoreResults $ result
      Nothing -> error "error parsing file"
