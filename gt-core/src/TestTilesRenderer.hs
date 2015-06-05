module TestTilesRenderer (
  TestTileRenderer
, TestMapRenderer

, renderTile
, renderMap
) where

import OpenGLRenderer
import Tiles
import TestTiles as TT
import TileRenderer

import Data.Map


type TestTileRenderer = TT.Tile -> IO()
type TestMapRenderer  = Renderer TT.Map

getVisibles :: TT.Tiles -> Camera Int -> [TT.Tile]
getVisibles tiles camera = elems $ filterWithKey inRange tiles
                        where inRange = \(Coordinate xx yy) _ ->    xx >= x tl && xx <= x br
                                                                 && yy >= y tl && yy <= y br
                              tl = topLeft camera
                              br = bottomRight camera

renderTile :: TestTileRenderer
renderTile tile@(Tiles.Tile id tpe state content) = do
    putStrLn $ show tile

renderMap :: Camera Int -> TestMapRenderer
renderMap = mkMapRenderer getVisibles renderTile
