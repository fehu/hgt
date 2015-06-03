module Main ( main ) where

import Tile

t = Tile "id" "type" "state" [1, 2, 3]

s = show t

main :: IO()
main = putStrLn s
