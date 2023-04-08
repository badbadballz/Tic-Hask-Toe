module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)
--import GHC.Base (magicDict)
--import Data.Monoid (First(getFirst))
--import GHC.Num (bitInteger)

-- *** Assignment 5-1 *** --

-- Q#01

printBoard :: Board -> IO()
printBoard b = putStr (formatBoard b)

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"


printLogo :: IO()
printLogo = do
             f <- readFile _LOGO_PATH_
             putStrLn f

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = do
                result <- _RANDOM_BOOL_
                return (getFirstPlayer result)
-- Q#04

getMove :: Board -> IO Move
getMove b = do 
              m <- getLine
              if isValidMove b (stringToMove m) then
                 return (stringToMove m)
              else do   
                    putStrLn "Invalid move! Try again"
                    getMove b
-- Q#05

play :: Board -> Player -> IO()
play b p = do
            when _DISPLAY_LOGO_ printLogo
            go b p
            where go b' p' = do
                                printBoard b'
                                putStrLn (promptPlayer p')
                                m <- getMove b'
                                let (gs, b'') = playMove p' b' m
                                        in case gs of 
                                            InProgress -> go b'' (switchPlayer p')
                                            _          -> do
                                                            printBoard b''
                                                            putStrLn (showGameState gs)


-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo = undefined

-- Q#08

firstPlayerDo = undefined

-- Q#09

getMoveDo = undefined

-- Q#10

playDo = undefined