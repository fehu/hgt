module Main ( main ) where

import OpenGLRenderer
import Tiles
import TileRenderer
import TestTiles as TT
import TestTilesRenderer
import Data.Map (fromList)


player1 = Owner "1" "xxPLAYERxx"
player2 = Owner "2" "__player__"


coords :: [TileId]
coords = [Coordinate x y | x <- [0, 1], y <- [0, 1]]

tls :: [TT.Tile]
tls = [
        Tile (Coordinate 0 0) Sea       (Nothing,      Storm)   []
      , Tile (Coordinate 0 1) Plain     (Just player1, Cloudy)  []
      , Tile (Coordinate 1 0) Hill      (Just player2, Rain)    []
      , Tile (Coordinate 1 1) Mountain  (Nothing,      Sunny)   []
      ]

neighbours :: Neighbours TileId
neighbours = fromList $ map f tls
          where f = \(Tile id _ _ _) -> (id, filter (id /=) coords)

type World = TT.Map

world :: World
world = Tiles.Map (mkTiles tls) neighbours


init :: Mutator World
init ref = sequence_ []

fixedCamera = Camera { topLeft      = Point 0 0
                     , bottomRight  = Point 1 1
                     }

render :: Renderer World
render = renderMap fixedCamera

main :: IO()
main = run Main.init render Nothing world
