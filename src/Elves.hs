module Elves
  ( maxCalories,
    topThreeCalories,
    ElvesNotes,
  )
where

import Data.List (sortOn)
import Data.Ord (Down (Down))

type ElvesNotes = [[Integer]]

maxCalories :: ElvesNotes -> Integer
maxCalories = maximum . map sum

topThreeCalories :: ElvesNotes -> Integer
topThreeCalories = sum . take 3 . sortOn Down . map sum
