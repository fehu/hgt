{-# LANGUAGE MultiParamTypeClasses
--           , FlexibleInstances
         #-}

module Vis (

  Renderer(..)
, Renderable(..)

) where


class Renderer r rc where
    bulkRender :: r -> rc -> IO()

class Renderable a r where
    renderWith :: r -> a -> IO()

