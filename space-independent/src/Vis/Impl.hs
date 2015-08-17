{-# LANGUAGE ExistentialQuantification #-}

module Vis.Impl (

) where

import Data.IORef

import Vis
import U.Objects
import U.Exec.Mutable

import OpenGLRenderer as R

data RenderableSystem sys d r =
    forall body obj. (Renderable (body d) r, Renderable (obj d) r, MutableSystem (sys body obj) body obj d) =>
     RenderableSystem (sys body obj d)

visualize :: (Vis.Renderer r (sys body obj d)) => r -> IO state -> RenderableSystem sys d r -> IO()
visualize r stateIO sys = R.run stateIO rend Nothing undefined undefined
                        where rend state wRef = do let l = do RenderableSystem w <- readIORef wRef
                                                              bulkRender r w
                                                   return l



