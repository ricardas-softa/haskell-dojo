module TTT.A1 where

import Data.Char (toUpper)

-- Q#01

_SIZE_ = 3

-- Q#02

_DISPLAY_LOGO_ = True

-- Q#03

convertRowIndex :: Char -> Int
convertRowIndex row = fromEnum ( toUpper row ) - 65
-- cleaner version
-- convertRowIndex = subtract 65 . fromEnum . toUpper

-- Q#04

_INVALID_MOVE_ = (-1, -1)

-- Q#05

_SEP_ = "_|_"

-- Q#06

data Square = X | O | E
  deriving (Eq, Show)

-- Q#07

data GameState = Xwon | Owon | Tie | InProgress
  deriving (Eq, Show)

-- Q#08

type Player = Square

type Row = [Square]

type Line = [Square]

type Board = [Row]

type Move = (Int, Int)

-- Q#09

getFirstPlayer :: Bool -> Player
getFirstPlayer val = if val then X else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ val | val = X
                    | otherwise = O

-- Q#10

showGameState :: GameState -> String
showGameState state = case state of
  Xwon -> "X won!"
  Owon -> "O won!"
  Tie -> "Tie!"
  InProgress -> "In progress..."

-- Q#11

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer E = E

-- Q#12

showSquare :: Square -> String
showSquare X = "X"
showSquare O = "O"
showSquare E = "_"