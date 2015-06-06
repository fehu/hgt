module TestTilesRenderer (
  TestTileRenderer
, TestMapRenderer

, renderTile
, renderMap

, glFloat, int2GLfloat
, glInt
, rgb2GLcolor
) where

import OpenGLRenderer
import Tiles
import TestTiles as TT
import TileRenderer as TR

import Data.Map
import GHC.Float.RealFracMethods

import Graphics.UI.GLUT



type TestTileRenderer = TT.Tile -> IO()
type TestMapRenderer  = Renderer TT.Map

getVisibles :: TT.Tiles -> Camera Int -> [TT.Tile]
getVisibles tiles camera = elems $ filterWithKey inRange tiles
                        where inRange = \(TR.Point xx yy) _ ->      xx >= x tl && xx <= x br
                                                                 && yy >= y tl && yy <= y br
                              tl = topLeft camera
                              br = bottomRight camera

beforeRender :: TT.Tile -> IO()
beforeRender t = do let c s = glFloat . int2Float . s . Tiles.id $ t
                    let cx = c x
                    let cy = c y

                    translate $ Vector3 cx cy 0
                    putStrLn $ "transtale: " ++ show cx  ++ ", " ++ show cy

renderTile :: TestTileRenderer
renderTile tile@(Tiles.Tile id tpe state content) = do
    putStrLn $ "rendering " ++ show tile
    renderPrimitive Quads $ do
        let (r, g, b) = case tpe of Plain       -> (0, 255, 0)
                                    Hill        -> (255, 255, 0)
                                    Mountain    -> (139, 69, 19)
                                    Sea         -> (0, 0, 255)
        color $ rgb2GLcolor r g b
        putStrLn $ "color: " ++ show r  ++ ", " ++ show g ++ ", " ++ show b

        let z = glFloat 1

        vertex $ Vertex3 (-1) (-1) z
        vertex $ Vertex3 1    (-1) z
        vertex $ Vertex3 1    1    z
        vertex $ Vertex3 (-1) 1    z

renderMap :: IO() -> IO() -> Camera Int -> TestMapRenderer
renderMap before after = mkMapRenderer before after beforeRender getVisibles renderTile





