module OpenGLRenderer (
  run
, Renderer
, Mutator

, Callbacks(..)
) where

import Data.IORef
import Graphics.UI.GLUT


type Renderer w = IORef w -> [DisplayCallback]
type Mutator  w = IORef w -> IO()

-- from https://mail.haskell.org/pipermail/beginners/2010-November/005709.html


data Callbacks state = Callbacks { callReshape :: Maybe (Window -> state -> ReshapeCallback)
                                 , callKey     :: Maybe (Window -> state -> KeyboardCallback)
                                 , callSpecKey :: Maybe (Window -> state -> SpecialCallback)
                                 }


run :: IO state -> (state -> Renderer w) -> Maybe (Mutator w) -> Callbacks state -> w -> IO()
run stateIO display change callbacks world = do
                        getArgsAndInitialize
                        _window <- createWindow "Test"

                        worldM <- newIORef world
                        state  <- stateIO

                        let applyCallback f = f _window state

                        reshapeCallback $= fmap applyCallback (callReshape callbacks)
                        displayCallback $= sequence_ (display state worldM)
                        idleCallback $= fmap ($ worldM) change

                        keyboardCallback $= fmap applyCallback (callKey callbacks)
                        specialCallback  $= fmap applyCallback (callSpecKey callbacks)

                        mainLoop

