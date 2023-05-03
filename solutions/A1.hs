module A1 where

import Data.Char (toUpper)
--import GHC.IO.Encoding (getFileSystemEncoding)
--import Data.Monoid (First(getFirst))
--import GHC.RTS.Flags (TickyFlags(showTickyStats))

-- *** Assignment 1-1 *** --

-- Q#01

_SIZE_ :: Int
_SIZE_ = 3

-- Q#02

_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03

convertRowIndex :: Char -> Int
convertRowIndex c = fromEnum (toUpper c) - 65

-- Q#04

_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05

_SEP_ :: String
_SEP_ = "_|_"

-- *** Assignment 1-2 *** --

-- Q#06
data Square = X | O | Empty deriving Eq

instance Show Square where
    show X = "X"
    show O = "O"
    show Empty = "_"
    
-- Q#07
data GameState = XWins | OWins | Tie | InProgress deriving (Eq, Show)


-- Q#08

type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)




-- Q#09

getFirstPlayer :: Bool -> Player
getFirstPlayer b = if b then X else O 

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ b
            | b = X
            | otherwise = O


-- Q#10

showGameState :: GameState -> String
showGameState gs = case gs of XWins -> "Player X wins!"
                              OWins -> "Player O wins!"
                              Tie -> "Tie!"
                              InProgress -> "Game is in progress"  

-- Q#11

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer Empty = Empty


-- Q#12
-- better solution?
showSquare :: Square -> String
showSquare = show

showSquare' :: Square -> String
showSquare' s = case s of X -> "X"
                          O -> "O"
                          Empty -> "_"