module RockPaperScisors
  ( parseMovesStrategyGuide,
    parseResultStrategyGuide,
    scoreMoves,
    scoreResults
  )
where

import Text.Read (readMaybe) -- Not sure if this is the best way to :: String -> Maybe Int

data Move = Rock | Paper | Scisors deriving (Show)
data Result = Win | Loose | Draw deriving (Show)

parseMovesStrategyGuide :: String -> Maybe [(Move, Move)]
parseMovesStrategyGuide = mapM (parseMovesStrategyGuideLine . words) . lines

parseMovesStrategyGuideLine :: [String] -> Maybe (Move, Move)
parseMovesStrategyGuideLine [a, b] =
  case (maybeOponent a, maybeMe b) of
    (Just opn, Just me) -> Just (opn, me)
    _ -> Nothing
parseMovesStrategyGuideLine _ = Nothing

parseResultStrategyGuide :: String -> Maybe [(Move, Result)]
parseResultStrategyGuide = mapM (parseResultStrategyGuideLine . words) . lines

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

scoreMoves :: [(Move, Move)] -> Integer
scoreMoves = sum . map scoreMove

scoreMove :: (Move, Move) -> Integer
scoreMove (oponent, me) = movePoints me + (resultPoints . resultFromMoves me $ oponent)

scoreResults :: [(Move, Result)] -> Integer
scoreResults = sum . map scoreResult

scoreResult :: (Move, Result) -> Integer
scoreResult (oponent, result) = resultPoints result + (movePoints . moveFromResult oponent $ result)

resultPoints :: Result -> Integer
resultPoints Loose = 0
resultPoints Draw = 3
resultPoints Win = 6

movePoints :: Move -> Integer
movePoints Rock = 1
movePoints Paper = 2
movePoints Scisors = 3

resultFromMoves :: Move -> Move -> Result
resultFromMoves Rock Paper = Loose
resultFromMoves Rock Scisors = Win
resultFromMoves Paper Rock = Win
resultFromMoves Paper Scisors = Loose
resultFromMoves Scisors Rock = Loose
resultFromMoves Scisors Paper = Win
resultFromMoves _ _ = Draw

moveFromResult :: Move -> Result -> Move
moveFromResult Rock Win = Paper
moveFromResult Rock Loose = Scisors
moveFromResult Paper Win = Scisors
moveFromResult Paper Loose = Rock
moveFromResult Scisors Win = Rock
moveFromResult Scisors Loose = Paper
moveFromResult move Draw = move
