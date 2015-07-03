module TestTiles (


  TestTiles.Tile
, TestTiles.Tiles
, TestTiles.Map

, TileId
, TileType(..)
, TileState
, TileWeather(..)
, TileContent(..)

, Coordinate(..)
, Owner(..)
, Unit(..)
) where

import Tiles
import Tiles.Renderer (Point)

type Coordinate = Point Int

type TileId = Coordinate

data TileType = Plain
              | Hill
              | Mountain
              | Sea
              deriving Show

data TileWeather = Sunny
                 | Cloudy
                 | Rain
                 | Snow
                 | Storm
                 deriving Show

data Owner = Owner {
                id   :: String
              , name :: String
              }
              deriving Show

type TileState = (Maybe Owner, TileWeather)

data Unit = Unit String
          deriving Show

data TileContent = City Int Owner String Int
                 | Units [Unit]
                 | Resource String Float
                 deriving Show


type Tile  = Tiles.Tile     TileId TileType TileState TileContent
type Tiles = Tiles.Tiles    TileId TileType TileState TileContent
type Map   = Tiles.Map      TileId TileType TileState TileContent


