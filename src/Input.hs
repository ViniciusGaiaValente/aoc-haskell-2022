module Input
  ( parseElvesNotes,
    parseMovesStrategyGuide,
    parseResultStrategyGuide,
  )
where

import Data.List.Split (splitWhen)
import Elves (ElvesNotes)
import RockPaperScisors (Move (..), Result (..))

parseElvesNotes :: String -> ElvesNotes
parseElvesNotes = map (map read) . splitWhen (== "") . lines

parseMovesStrategyGuide :: String -> Maybe [(Move, Move)]
parseMovesStrategyGuide = mapM (parseMovesStrategyGuideLine . words) . lines

parseResultStrategyGuide :: String -> Maybe [(Move, Result)]
parseResultStrategyGuide = mapM (parseResultStrategyGuideLine . words) . lines

parseMovesStrategyGuideLine :: [String] -> Maybe (Move, Move)
parseMovesStrategyGuideLine [a, b] =
  case (maybeOponent a, maybeMe b) of
    (Just opn, Just me) -> Just (opn, me)
    _ -> Nothing
parseMovesStrategyGuideLine _ = Nothing

parseResultStrategyGuideLine :: [String] -> Maybe (Move, Result)
parseResultStrategyGuideLine [a, b] =
  case (maybeOponent a, maybeResult b) of
    (Just opn, Just res) -> Just (opn, res)
    _ -> Nothing
parseResultStrategyGuideLine _ = Nothing

maybeOponent :: String -> Maybe Move
maybeOponent "A" = Just Rock
maybeOponent "B" = Just Paper
maybeOponent "C" = Just Scisors
maybeOponent _ = Nothing

maybeMe :: String -> Maybe Move
maybeMe "X" = Just Rock
maybeMe "Y" = Just Paper
maybeMe "Z" = Just Scisors
maybeMe _ = Nothing

maybeResult :: String -> Maybe Result
maybeResult "X" = Just Loose
maybeResult "Y" = Just Draw
maybeResult "Z" = Just Win
maybeResult _ = Nothing
