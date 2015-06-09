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
coords = [Point x y | x <- [0, 5], y <- [0, 1]]
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
      , Tile (Point 1 1) Sea       (Nothing,      Storm)   []
      , Tile (Point 2 0) Sea       (Nothing,      Rain)    []
      , Tile (Point 2 1) Sea       (Nothing,      Storm)   []
      , Tile (Point 3 0) Sea       (Nothing,      Rain)    []
      , Tile (Point 3 1) Plain     (Just player1, Cloudy)  []
      , Tile (Point 4 0) Hill      (Just player2, Rain)    []
      , Tile (Point 5 1) Mountain  (Nothing,      Sunny)   []
      ]

neighbours :: Neighbours TileId
neighbours = fromList $ map f tls
          where f = \(Tile id _ _ _) -> (id, filter (id /=) coords)

type World = TT.Map

world :: World
world = Tiles.Map (mkTiles tls) Main.neighbours


reshaped ::  ReshapeCallback --Mutator World
reshaped size = do putStrLn $ "viewport"
                   viewport $= (mkPosition 0 0, mkSize 400 400) -- size
                   postRedisplay Nothing

fixedCamera = Camera { topLeft      = Point 0 0
                     , bottomRight  = Point 2 2
                     }

mkPosition :: Int -> Int -> Position
mkPosition x y = Position (glInt x) (glInt y)

mkSize :: Int -> Int -> Size
mkSize w h = Size (glInt w) (glInt h)


keyCallbacks :: Maybe (Window -> KeyboardCallback)
keyCallbacks = Just $ \w -> \key pos -> case key of '\ESC' -> do destroyWindow w
                                                    _      -> return () --putStrLn $ "key " ++ show key

keySpecialCallbacks :: Maybe (Window -> SpecialCallback)
keySpecialCallbacks = Just $ \w -> \key pos -> case key of KeyUp    -> fail "up"
                                                           KeyRight -> fail "right"
                                                           KeyDown  -> fail "down"
                                                           KeyLeft  -> fail "left"
                                                           _        -> return ()

render :: Renderer World
render = renderMap before after fixedCamera
      where before = do clear [ColorBuffer]
                        loadIdentity
                        color $ rgb2GLcolor 255 255 255
                        renderPrimitive Quads $ tileQuad (1 :: GLfloat)
            after  = flush

callbacks = Callbacks{ callReshape = Nothing
                     , callKey = keyCallbacks
                     , callSpecKey = keySpecialCallbacks
                     }

main :: IO()
main = run render Nothing callbacks world
