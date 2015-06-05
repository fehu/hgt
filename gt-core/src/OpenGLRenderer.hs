module OpenGLRenderer (
  run
, Renderer
, Mutator
) where

import Data.IORef
import Graphics.UI.GLUT


type Renderer w = IORef w -> IO()
type Mutator  w = IORef w -> IO()

-- from https://mail.haskell.org/pipermail/beginners/2010-November/005709.html

run :: Mutator w -> Renderer w -> Maybe (Mutator w) -> w -> IO()
run init display change world = do  getArgsAndInitialize
                                    _window <- createWindow "Test"

                                    worldM <- newIORef world
                                    init worldM

                                    displayCallback $= (display worldM)
                                    idleCallback $= fmap ($ worldM) change

                                    mainLoop

