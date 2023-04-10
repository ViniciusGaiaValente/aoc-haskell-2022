module DayOne
  ( partOne,
    partTwo,
  )
where

import Elves
  ( maxCalories,
    topThreeCalories,
  )
import Input (parseElvesNotes)

elvesNotesPath :: FilePath
elvesNotesPath = "./elves_notes.txt"

partOne :: IO Integer
partOne =
  do
    fileContent <- readFile elvesNotesPath
    return . maxCalories . parseElvesNotes $ fileContent

partTwo :: IO Integer
partTwo =
  do
    fileContent <- readFile elvesNotesPath
    return . topThreeCalories . parseElvesNotes $ fileContent
