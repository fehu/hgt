module Tiles (
  Tile(..)

, Tiles
, Neighbours

, Tiles.Map(..)
, tiles, tile
, mkTiles
) where

import Data.Map as DMap
import Data.List (intercalate)


data Tile  id tpe state content = Tile {
                                         id :: id
                                       , tpe :: tpe
                                       , state :: state
                                       , contents :: [content]
                                       }

type Tiles id tpe state content = DMap.Map id (Tile id tpe state content)

type Neighbours id = DMap.Map id [id]

data Map id tpe state content = Map (Tiles id tpe state content) (Neighbours id)

tiles :: Tiles.Map id tpe state content -> [Tile id tpe state content]
tiles (Map ts ns) = elems ts

tile  :: (Ord id) => id -> Tiles.Map id tpe state content -> Maybe (Tile id tpe state content)
tile id (Map ts ns) = DMap.lookup id ts

instance (Show id, Show tpe, Show state, Show content) => Show (Tile id tpe state content) where
    show (Tile id tpe state contents) = "Tile(" ++ str ++ ")"
                                     where str = intercalate " " [ show id
                                                                 , show tpe
                                                                 , show state
                                                                 , show contents
                                                                 ]


mkTiles :: Ord id => [Tile id tpe state content] -> Tiles id tpe state content
mkTiles tls = DMap.fromList $ Prelude.map (\t@(Tile id _ _ _) -> (id, t)) tls

