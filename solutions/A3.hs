module A3 where

import A1
import A2

import Data.List (transpose)
--import System.Posix.Internals (puts)
--import Data.Monoid (All(getAll))

-- *** Assignment 3-1 ***

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

showInts' :: [Int] -> [String]
showInts' [] = []
showInts' xs = map show xs


_HEADER_ = formatLine $ showInts _RANGE_

-- Q#02

showSquares :: [Player] -> [String]
showSquares [] = []
showSquares (x:xs) = show x : showSquares xs


-- Q#03
formatRows :: Board -> [String]
formatRows [] = []
formatRows (b:bs) = formatLine (showSquares b) : formatRows bs

formatRows' :: Board -> [String]
formatRows' b = let b' = map showSquares b
                in map formatLine b'

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty r i 
    | elem i _RANGE_ = r !! i == Empty 
    | otherwise = False

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol [[]] = [[]]
dropFirstCol b = map tail b

dropLastCol :: Board -> Board
dropLastCol [[]] = [[]]
dropLastCol b = map init b

-- Q#06

getDiag1 :: Board -> Line
getDiag1 [[]] = []
getDiag1 b = go b 0 
                where go [] _ = []
                      go (b':bs') i = b' !! i : go bs' (i + 1) 


getDiag2 :: Board -> Line
getDiag2 [[]] = []
getDiag2 b = go b (_SIZE_ - 1)
                where go [] _ = []
                      go (b':bs') i 
                            | i >= 0 = b' !! i : go bs' (i - 1)
                            | otherwise = [] -- preventing negative index


getAllLines :: Board -> Board
getAllLines b = let t = transpose b
                    d1 = getDiag1 b
                    d2 = getDiag2 b
                    in b ++ t ++ [d1] ++ [d2]


-- *** Assignment 3-2 ***

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare p b m
        | isMoveInBounds m = go p b m 0 
        | otherwise = b
            where go _ [] _ _ = []
                  go p' (b:bs) (r, c) i 
                        | r == i = let updatedRow = replaceSquareInRow p' c b
                                    in updatedRow : bs
                        | otherwise = b : go p' bs m (i + 1)
     

-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices s = go $ indexRowStrings s
                         where go [] = []
                               go ((i, x) : xs) = let prepended = i:x 
                                                    in prepended : go xs   

test1 = [". Learn Haskell", ". Wait for more industry adoption", ". Profit?"]
-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine p l = go l False
                     where  go [] b = b 
                            go (x : xs) b = if x == p then go xs True
                                            else False

test2 = [X, X ,X]
test3 = [O, O ,O, O]
test4 = [X, X, O]
test5 = [X, O, O]
-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove [] _  = False
isValidMove b m@(r, c) 
    | isMoveInBounds m = let row = findRow b 0
                            in isColEmpty row c                           
    | otherwise = False 
        where findRow [] _ = []
              findRow (b':bs') i =  if r == i then b' else findRow bs' (i + 1)

-- can use !! instead??
--test6 = [X, Empty, O]
--test7 = [Empty, Empty, O]