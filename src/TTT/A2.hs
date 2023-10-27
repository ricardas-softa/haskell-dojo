module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01

promptPlayer :: Player -> String
promptPlayer player =
  concat
    [ "Player ",
      show player,
      "'s turn: enter a row and column position (ex. A1)"
    ]

-- Q#02
_RANGE_ = [0 .. _SIZE_ - 1]

-- Q#03

isDigit :: Char -> Bool
isDigit val = val `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit val
  | isDigit val = read [val]
  | otherwise = -1

-- Q#04

_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied b = notElem E $ concat b

_TIED_BOARD_ :: Board
_TIED_BOARD_ =
  [ [X, O, O],
    [O, X, X],
    [O, X, O]
  ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings list = zip ['A' ..] list

-- Q#07

formatLine :: [String] -> String
formatLine list = _SEP_ ++ intercalate _SEP_ list ++ _SEP_

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (row, col) = and [row >= 0, row < _SIZE_, col >= 0, col < _SIZE_]

isMoveInBounds' :: Move -> Bool
isMoveInBounds' (row, col) = row `elem` _RANGE_ && col `elem` _RANGE_  

-- Q#09

stringToMove :: String -> Move
stringToMove [] = _INVALID_MOVE_
stringToMove [_] = _INVALID_MOVE_
stringToMove [x,y] = (convertRowIndex x, readDigit y)
stringToMove _ = _INVALID_MOVE_

-- Q#10

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow pl col row | col `notElem` _RANGE_ || null row = row
                              | otherwise =
                                let (a,b) = splitAt col row
                                in a ++ [pl] ++ tail b

rsX :: Int -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Int -> Row -> Row
rsO = replaceSquareInRow O