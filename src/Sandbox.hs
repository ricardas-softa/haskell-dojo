module Sandbox where

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = sum' xs + x

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = length' xs + 1

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' val (x:xs) = val == x || elem' val xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

last' :: [a] -> a
last' [] = error "EMPTY LIST"
last' [a] = a
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "EMPTY LIST"
init' [a] = []
init' (x:xs) = x : init' xs


-- reverse with recursive-go

reverse' :: [a] -> [a]
reverse' xs = go [] xs where
    go acc [] = acc
    go acc (y:ys) = go (y:acc) ys