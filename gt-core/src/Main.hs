module Main ( main ) where

import OpenGLRenderer
import Tiles
import TileRenderer
import TestTiles as TT
import TestTilesRenderer
import Data.Map (fromList)

import Graphics.UI.GLUT hiding (Point)

player1 = Owner "1" "xxPLAYERxx"
player2 = Owner "2" "__player__"


coords :: [TileId]
coords = [Point x y | x <- [0, 1], y <- [0, 1]]
--coords = do
--    x <- [0, 1]
--    y <- [0, 1]
--    return $ Coordinate x y
--coords = [0, 1] >>= (
--    \x -> [0, 1] >>= (
--        \y ->
--            return $ Coordinate x y
--        )
--    )

tls :: [TT.Tile]
tls = [
        Tile (Point 0 0) Sea       (Nothing,      Storm)   []
      , Tile (Point 0 1) Sea       (Nothing,      Storm)   []
      , Tile (Point 1 0) Sea       (Nothing,      Rain)    []
      , Tile (Point 1 1) Plain     (Just player1, Cloudy)  []
      , Tile (Point 2 0) Hill      (Just player2, Rain)    []
      , Tile (Point 2 1) Mountain  (Nothing,      Sunny)   []
      ]

neighbours :: Neighbours TileId
neighbours = fromList $ map f tls
          where f = \(Tile id _ _ _) -> (id, filter (id /=) coords)

type World = TT.Map

world :: World
world = Tiles.Map (mkTiles tls) Main.neighbours


reshaped ::  ReshapeCallback --Mutator World
reshaped size = do putStrLn $ "viewport"
                   viewport $= (mkPosition 0 0, mkSize 200 200) -- size
                   postRedisplay Nothing

fixedCamera = Camera { topLeft      = Point 0 0
                     , bottomRight  = Point 2 2
                     }

mkPosition :: Int -> Int -> Position
mkPosition x y = Position (glInt x) (glInt y)

mkSize :: Int -> Int -> Size
mkSize w h = Size (glInt w) (glInt h)


render :: Renderer World
render = renderMap before after fixedCamera
      where before = do let sc = 1.3e-3
--                        scale sc sc (sc :: GLfloat)
                        clear [ColorBuffer]
            after  = flush

main :: IO()
main = run render Nothing (Just reshaped) world
