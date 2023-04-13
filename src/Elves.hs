module Elves
  ( parseElvesNotes,
    maxCalories,
    topThreeCalories,
  )
where

import Data.List (sortOn)
import Data.List.Split (splitWhen)
import Data.Ord (Down (Down))
import Text.Read (readMaybe) -- Not sure if this is the best way to :: String -> Maybe Int

type ElvesNotes = [[Integer]]

parseElvesNotes :: String -> Maybe ElvesNotes
parseElvesNotes = mapM (mapM readMaybe) . splitWhen (== "") . lines

maxCalories :: ElvesNotes -> Integer
maxCalories = maximum . map sum

topThreeCalories :: ElvesNotes -> Integer
topThreeCalories = sum . take 3 . sortOn Down . map sum
