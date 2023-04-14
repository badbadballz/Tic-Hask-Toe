{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)

-- *** Assignment 2-1 *** --

-- Q#01
--"Player X's turn: enter a row and column position (ex. A1)"

promptPlayer :: Player -> String
promptPlayer p = "Player " ++ show p ++ "'s turn: enter a row and column position (ex. A1)"

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0 .. (_SIZE_ - 1)]

-- Q#03
isDigit :: Char -> Bool
isDigit c = let range = ['0'..'9'] 
                in elem c range
                              
readDigit :: Char -> Int
readDigit c  
    | isDigit c = read [c] :: Int
    | otherwise = -1

-- Q#04

_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

_TIED_BOARD2_ :: Board
_TIED_BOARD2_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, Empty]
  ]

isTied :: Board -> Bool
isTied b = notElem Empty $ concat b

-- Q#06
indexRowStrings :: [String] -> [(Char, String)] 
indexRowStrings = zip ['A'.. ]

-- Q#07

formatLine :: [String] -> String
formatLine s = let middle = intercalate _SEP_ s
                in _SEP_ ++ middle ++ _SEP_
                
-- *** Assignment 2-2 *** --

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (r, c) = elem r _RANGE_ && elem c _RANGE_

-- Q#09
stringToMove :: String -> Move
stringToMove [a, b] = (convertRowIndex a, readDigit b)
stringToMove _ = _INVALID_MOVE_

{- this only works for string of exactly 2 characters long, so for otherwise legal moves like A11 
then it would still return an invalid move -}
stringToMove' :: String -> Move
stringToMove' s 
    | length s == 2 = (convertRowIndex $ head s , readDigit $ last s)
    | otherwise = _INVALID_MOVE_
-- Q#10

{-}
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow p c r = case p of X -> rsX c r 
                                            where
                                                rsX _ [] = []
                                                rsX c' r' 
                                                 | c' < 0 || c' > (_SIZE_ - 1) = r'
                                                 | otherwise = let (s1, s2) = splitAt c' r
                                                                in  s1 ++ [p] ++ tail s2 
                                      O -> rsO c r
                                            where 
                                                rsO _ [] = []
                                                rsO c' r' 
                                                 | c' < 0 || c' > (_SIZE_ - 1) = r'
                                                 | otherwise = let (s1, s2) = splitAt c' r
                                                                in  s1 ++ [p] ++ tail s2 
                                      Empty -> r -- not sure if an Empty Player would require row to be updated?
-}

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow _ _ [] = []
replaceSquareInRow Empty _ r = r
replaceSquareInRow p c r 
        | c < 0 || c > (_SIZE_ - 1) = r
        | otherwise = let (s1, s2) = splitAt c r
                         in  s1 ++ [p] ++ tail s2

rsX :: Int -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Int -> Row -> Row
rsO = replaceSquareInRow O

e = head _EMPTY_BOARD_
t = last _TIED_BOARD_