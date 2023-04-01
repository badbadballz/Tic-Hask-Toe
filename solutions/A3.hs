module A3 where

import A1
import A2

import Data.List (transpose)
--import Data.Monoid (All(getAll))

-- *** Assignment 3-1 ***

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts xs = map show xs


_HEADER_ = formatLine $ showInts _RANGE_

-- Q#02

showSquares :: [Player] -> [String]
showSquares [] = []
showSquares (x:xs) = show x : showSquares xs


-- Q#03
formatRows :: Board -> [String]
formatRows b = let b' = map showSquares b
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
                      go (b':bs') i = b' !! i : go bs' (i - 1)


getAllLines :: Board -> Board
getAllLines b = let t = transpose b
                    d1 = getDiag1 b
                    d2 = getDiag2 b
                    in b ++ t ++ [d1] ++ [d2]


-- *** Assignment 3-2 ***

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined