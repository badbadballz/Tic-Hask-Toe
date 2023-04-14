module Main where

import A1
import A2
import A3
import A4
import A5

main :: IO ()
main = do 
          p <- firstPlayer
          playDo _EMPTY_BOARD_ p
    
    
--putStrLn "Welcome to Part I of EMURGO Academy's Haskell course!"
