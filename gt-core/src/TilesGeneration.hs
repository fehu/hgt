module TilesGeneration (

) where

import Tiles
import Utils
import GHC.Ptr

type Error = String

type Generation a = a -> Either a Error

data MapSectionBuilder coord content = MapSectionBuilder{ mapSection :: Rect coord
                                                        , sectionContents :: [[Ptr content]]
                                                        }

class MapModifier m where

data GenerationStage a = GenerationStage { generationStageName :: String
                                         , generationStageFunc :: Generation a
                                         }


