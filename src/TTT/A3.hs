module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import Data.Functor.Classes (showsUnaryWith)
import Data.Graph (indegree)
import Foreign (toBool)

-- Q#01

-- showInts :: [Int] -> [String]
-- simplest
-- showInts = map show

-- as ordered (manually)
showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

_HEADER_ = ' ' : formatLine (showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x:xs) = showSquare x : showSquares xs

-- Q#03

formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (x:xs) = formatLine ( showSquares x ) : formatRows xs

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty (x:_) 0 = x == E
isColEmpty (_:xs) n = isColEmpty xs (n-1)

-- Q#05

-- dropFirstCol :: Board -> Board
-- dropFirstCol = map tail -- simple and elegant

dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (x:xs) = tail x : dropFirstCol xs

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (x:xs) = init x : dropLastCol xs

-- Q#06

getDiag1 :: Board -> Line   -- from top left to bottom right
getDiag1 [] = []
getDiag1 (x:xs) = head x : getDiag1 (dropFirstCol xs)

getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 (x:xs) = last x : getDiag2 (dropLastCol xs)

getAllLines :: Board -> [Line]
getAllLines board = concat [board, transpose board, [getDiag1 board, getDiag2 board]]

-- Q#07

-- putSquare :: Player -> Board -> Move -> Board
-- putSquare p b m = go p b m [] where
--     go :: Player -> Board -> Move -> Board -> Board
--     go _ [] _ _ = []
--     go p (l:ls) (x,y) board | y == length board = go p ls (x,y) board ++ [replaceSquareInRow p x l]
--                             | otherwise go p ls (x,y) board ++ l

putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare p (l:ls) (x,y) | y == 0 = replaceSquareInRow p x l : ls
                         | otherwise = l : putSquare p ls (x,y-1)

-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices list = go [] (indexRowStrings list) where
    go :: [String] -> [(Char, String)] -> [String]
    go acc [] = acc
    go acc ((c,s):xs) = go (acc ++ [c:s]) xs

-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine p l = go True p l where
    go :: Bool -> Player -> Line -> Bool
    go _ _ [] = False
    go _ p [x] = p == x
    go acc p (x:xs) = go (acc && p == x) p xs


-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove b m
  | not (isMoveInBounds m) = False
  | otherwise = go b 0 m
  where
    go :: Board -> Int -> Move -> Bool
    go [] _ _ = False
    go (x:xs) i (r, c)
      | i == r = isColEmpty x c
      | otherwise = go xs (i + 1) (r, c)
