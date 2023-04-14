module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)

-- *** Assignment 5-1 *** --

-- Q#01

printBoard :: Board -> IO()
printBoard b = putStr (formatBoard b)

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"


printLogo :: IO()
printLogo = readFile _LOGO_PATH_ >>= putStrLn 

{- printLogo = do
             f <- readFile _LOGO_PATH_
             putStrLn f
-}
-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= return.getFirstPlayer
{- firstPlayer = do
                result <- _RANDOM_BOOL_
                return (getFirstPlayer result)
-}
-- Q#04

getMove :: Board -> IO Move
getMove b = getLine >>=
            (\m -> if isValidMove b (stringToMove m) then
                      return (stringToMove m)
                      else 
                        putStrLn "Invalid move! Try again" >>
                        getMove b)
{- getMove b = do 
              m <- getLine
              if isValidMove b (stringToMove m) then
                 return (stringToMove m)
              else do   
                    putStrLn "Invalid move! Try again"
                    getMove b
-}
-- Q#05

play :: Board -> Player -> IO()
play b p = when _DISPLAY_LOGO_ printLogo >>
           go b p
           where 
            go b' p' = printBoard b' >>
                       putStrLn (promptPlayer p') >>
                       getMove b' >>=
                       (\m -> 
                        let (gs, b'') = playMove p' b' m
                           in case gs of
                                 InProgress -> go b'' (switchPlayer p')
                                 _          -> printBoard b'' >> putStrLn (showGameState gs)
                        )
                       
{- play b p = do
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
-}

-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo :: IO()
printLogoDo = do
                 f <- readFile _LOGO_PATH_
                 putStrLn f
--printLogoDo = undefined

-- Q#08
firstPlayerDo :: IO Player
firstPlayerDo = do
                  result <- _RANDOM_BOOL_
                  return (getFirstPlayer result)

--firstPlayerDo = undefined

-- Q#09
getMoveDo :: Board -> IO Move
getMoveDo b = do 
              m <- getLine
              if isValidMove b (stringToMove m) then
                 return (stringToMove m)
              else do   
                    putStrLn "Invalid move! Try again"
                    getMove b

--getMoveDo = undefined

-- Q#10
playDo :: Board -> Player -> IO()
playDo b p = do
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
--playDo = undefined