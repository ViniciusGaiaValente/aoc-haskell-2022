module Input (parseElvesNotes) where

import Data.List.Split (splitWhen)
import Elves (ElvesNotes)

parseElvesNotes :: String -> ElvesNotes
parseElvesNotes = map (map read) . splitWhen (== "") . lines
