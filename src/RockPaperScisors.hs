module RockPaperScisors
  ( scoreMoves,
    scoreResults,
    Move (..),
    Result (..),
  )
where

data Move = Rock | Paper | Scisors deriving (Show)

data Result = Win | Loose | Draw deriving (Show)

scoreMoves :: [(Move, Move)] -> Integer
scoreMoves = sum . map scoreMove

scoreResults :: [(Move, Result)] -> Integer
scoreResults = sum . map scoreResult

scoreMove :: (Move, Move) -> Integer
scoreMove (oponent, me) = movePoints me + (resultPoints . resultFromMoves me $ oponent)

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
