module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )
import Text.Printf (PrintfArg(formatArg))

-- *** Assignment 4-1 *** --

-- Q#01

_HEADER_ = " " ++ (formatLine $ map show _RANGE_)

-- Q#02

showSquares s = map show s

-- Q#03

--dropFirstCol = undefined
dropFirstCol :: Board -> Board
dropFirstCol [[]] = [[]]
dropFirstCol b = map tail b


-- Q#04

--dropLastCol = undefined
dropLastCol :: Board -> Board
dropLastCol [[]] = [[]]
dropLastCol b = map init b
--Q#05

formatRows :: Board -> [String]
formatRows [] = []
formatRows b = map (formatLine . \r -> showSquares r) b

-- Q#06
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ p l = let result = filter (/=p) l
                      in null result

-- *** Assignment 4-2 *** --

-- Q#07
isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine p l = foldr (\x a -> if a then p == x else a) True l 

-- Q#08

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

hasWon :: Player -> Board -> Bool
hasWon _ [] = False
hasWon p b = let allLines = getAllLines b
              in foldr (\x a -> isWinningLine p x || a) False allLines

-- Q#09

getGameState :: Board -> GameState
getGameState b 
    | hasWon X b = XWins
    | hasWon O b = OWins
    | hasTied b = Tie
    | otherwise = InProgress  
      where hasTied b' = let checkedEmpty = map (notElem Empty) b'
                          in foldr (\x a -> x && a) True checkedEmpty


playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = let b' = putSquare p b m
                  in (getGameState b', b')

-- Q#10

prependRowIndices :: [String] -> [String]
prependRowIndices s = zipWith (\a b -> a : b) ['A'..] s

-- Q#11

formatBoard :: Board -> String
formatBoard b = let prepended = _HEADER_ : prependRowIndices (formatRows b)
                  in unlines prepended