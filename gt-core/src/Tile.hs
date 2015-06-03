
module Tile (
  Tile(..)

, Tiles
, Neighbours

, Tile.Map()
, tiles, tile
) where

import Data.Map as DMap


data Tile  id tpe state content = Tile id tpe state [content]

type Tiles id tpe state content = DMap.Map id (Tile id tpe state content)

type Neighbours id = DMap.Map id [id]

data Map id tpe state content = Map (Tiles id tpe state content) (Neighbours id)

tiles :: (Tile.Map id tpe state content) -> [Tile id tpe state content]
tiles (Map ts ns) = elems ts

tile  :: (Ord id) => id -> (Tile.Map id tpe state content) -> Maybe (Tile id tpe state content)
tile id (Map ts ns) = DMap.lookup id ts

instance (Show id, Show tpe, Show state, Show content) => Show (Tile id tpe state content) where
    show (Tile id tpe state contents) = Prelude.foldr (\x acc -> x ++ " " ++ acc) "" $
                                            [show id, show tpe, show state, show contents]


mkString :: (Show a) => String -> [a] -> String
mkString sep list = Prelude.foldr (\x acc -> show x ++ sep ++ acc) "" list



--data TileRender d = TileRender [TileRenderLayer d]
--data TileRenderLayer d = TileRenderLayer [d]
--
--data TileRenderCmd d = TileRenderPrepend d
--                     | TileRenderAppend  d
--                     | TileRenderInsert  Int
--                     | TileRenderRm      Int
--                     | TileRenderCmds    [TileRenderCmd d]
--
--instance Monad TileRenderCmd where
--    (>>) (TileRenderCmds cmds) cmd = TileRenderCmds [cmd]
