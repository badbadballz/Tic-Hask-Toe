module Sandbox where
--import Data.Bits (Bits(xor))
--import Control.Arrow (Arrow(first))

type Name = String
type Id = Int
type Power = String

data Pokemon = MkPokemon Name Id [Power] deriving Show

pikachu = MkPokemon "Pikachu" 20 ["Electric"]
bulbasaur = MkPokemon "Bulbasaur" 25 ["Grass", "Poison"]

getFirstPower :: Pokemon -> Power
getFirstPower (MkPokemon _ _ []) = [] 
getFirstPower (MkPokemon _ _ (x:xs)) = x

fifth :: [a] -> a
fifth xs = if length xs < 5 then head xs else go xs 
                where go (_ : (_ : (_ : (_ : (x : _))))) = x
