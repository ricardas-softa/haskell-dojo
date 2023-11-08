module TTT.A4 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)

-- Q#01

showInts :: [Int] -> [String]
showInts = map show

_HEADER_ = ' ' : formatLine (showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares = map showSquare

-- Q#03

dropFirstCol :: Board -> Board
dropFirstCol = map tail

-- Q#04

dropLastCol :: Board -> Board
dropLastCol = map init

--Q#05

formatRows :: [Row] -> [String]
formatRows = map (formatLine . showSquares)

-- Q#06

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ p l = null $ filter (/= p) l

-- Q#07

isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine p l = foldr (\x acc -> x == p && acc) True l

-- Q#08

hasWon :: Player -> Board -> Bool
hasWon p b = foldr (\l acc -> acc || isWinningLine p l) False $ getAllLines b

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

-- Q#09

getGameState :: Board -> GameState
getGameState [] = InProgress
getGameState b | hasWon X b = Xwon
               | hasWon O b = Owon
               | isTied b = Tie
               | otherwise = InProgress


playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = let newBoard = putSquare p b m
    in (getGameState newBoard, newBoard)

-- Q#10

-- prependRowIndices :: [String] -> [String]
-- prependRowIndices list = go [] (indexRowStrings list) where
--     go :: [String] -> [(Char, String)] -> [String]
--     go acc [] = acc
--     go acc ((c,s):xs) = go (acc ++ [c:s]) xs

prependRowIndices :: [String] -> [String]
prependRowIndices = zipWith (:) ['A'..]

-- Q#11

formatBoard :: Board -> String
formatBoard = unlines . (_HEADER_:) .  prependRowIndices . formatRows