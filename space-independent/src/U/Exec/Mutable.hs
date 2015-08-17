{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
         #-}

module U.Exec.Mutable (

  BasicSystemInterface(..)
, BasicSystem(..)

, MutableSystemInterface(..)
, MutableSystem(..)

--, SystemMExec(..)

, MSystemEntry(..)
, MSystem(..)

) where


import Data.UUID
import Data.Map as Map hiding (map)
import Data.Functor ((<$>))


import Control.Monad (join)
--import GHC.Ptr
--import Foreign.Storable
import Data.IORef

import U.Objects as O
import U.Defs    as D
import U.Exec


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class BasicSystemInterface sys body d where
    list     :: sys d           -> IO [body d]
    fullList :: sys d           -> IO [(body d, BodyState d)]
    get      :: sys d -> body d -> IO (body d, Vector d, Position d)


class (Body body d, BasicSystemInterface sys body d) => MutableSystemInterface sys body d where
    add :: sys d -> body d -> BodyState d -> IO()
    rm  :: sys d -> body d                -> IO()

    upd :: sys d -> body d -> BodyState d -> IO()

    updBody     :: sys d -> body d -> (body d -> body d)         -> IO (body d)
    updImpulse  :: sys d -> body d -> (Vector d -> Vector d)     -> IO (Vector d)
    updPosition :: sys d -> body d -> (Position d -> Position d) -> IO (Position d)

--class MutableSystem body obj d where
--    addStellar :: body d -> BodyState d -> IO()
--    rmStellar  :: body d -> IO()
--    getStellar :: body d -> IO (body d)
--    updStellar :: body d -> (body d -> body d) -> IO (body d)
--    updStellarImpulse  :: body d -> (Vector d -> Vector d) -> IO (Vector d)
--    updStellarPosition :: body d -> (Position d -> Position d) -> IO (Position d)

class (BasicSystemInterface sys body d,   BasicSystemInterface sys obj d)   => BasicSystem sys body obj d
class (MutableSystemInterface sys body d, MutableSystemInterface sys obj d) => MutableSystem sys body obj d

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data MSystemEntry a d = MSystemEntry { mPtr      :: IORef (a d)
                                     , mImpulse  :: IORef (Vector d)
                                     , mPosition :: IORef (Position d)
                                     }

data MSystem body obj d = MSystem { mSystemId      :: UUID
                                  , mStellarBodies :: IORef (Map UUID (MSystemEntry body d))
                                  , mArtificial    :: IORef (Map UUID (MSystemEntry obj  d))
                                  }

mLookupStellarEntry sys x = do bodies <- readIORef $ mStellarBodies sys
                               return $ Map.lookup (D.id x) bodies

mLookupArtificialEntry sys x = do bodies <- readIORef $ mArtificial sys
                                  return $ Map.lookup (D.id x) bodies

meToTriple se = do obj <- readIORef $ mPtr se
                   imp <- readIORef $ mImpulse se
                   pos <- readIORef $ mPosition se
                   return (obj, imp, pos)




_get f sys body = do bodies <- readIORef $ f sys
                     maybe (error "A") meToTriple (Map.lookup (D.id body) bodies)

mFList f g sys = do bodies <- readIORef $ g sys
                    let l = do entry <- elems bodies
                               return . f $ entry
                    sequence l

mList = mFList (readIORef . mPtr)

mListEntries = mFList meToTriple

mListFull g = do entries <- mListEntries g
                 return $ fmap (map f) entries
           where f (x, y, z)= (x, (y, z))

meUpd sel selE sys x upd = do Just entry <- sel sys x
                              let ptr = selE entry
                              old <- readIORef ptr
                              let new  = upd old
                              writeIORef ptr new
                              return new

mUpdState sel sys x state = do Just entry <- sel sys x
                               writeIORef (mImpulse  entry) (fst state)
                               writeIORef (mPosition entry) (snd state)

mRm sel sys x = do let bodiesRef = sel sys
                   bodies <- fmap (Map.delete (D.id x)) (readIORef bodiesRef)
                   writeIORef bodiesRef bodies

mAdd sel sys x state = do let bodiesRef = sel sys
                          bodies <- readIORef bodiesRef

                          let id = D.id x
                          xRef   <- newIORef x
                          impRef <- newIORef $ fst state
                          posRef <- newIORef $ snd state

                          let entry = MSystemEntry xRef impRef posRef
                          let newBodies = Map.insert id entry bodies
                          writeIORef bodiesRef newBodies


instance (Body body d)             => BasicSystemInterface   (MSystem body obj) body     d where
                                        get  = _get mStellarBodies
                                        list = mList mStellarBodies
                                        fullList = mListFull mStellarBodies

instance (Body obj d)              => BasicSystemInterface   (MSystem body obj) obj      d where
                                        get = _get mArtificial
                                        list = mList mArtificial
                                        fullList = mListFull mArtificial

instance (Body body d, Body obj d) => BasicSystem            (MSystem body obj) body obj d


instance (Body body d)             => MutableSystemInterface (MSystem body obj) body     d where
                                        add = mAdd mStellarBodies
                                        rm  = mRm  mStellarBodies
                                        upd = mUpdState mLookupStellarEntry
                                        updBody     = meUpd mLookupStellarEntry mPtr
                                        updImpulse  = meUpd mLookupStellarEntry mImpulse
                                        updPosition = meUpd mLookupStellarEntry mPosition

instance (Body body d, Body obj d) => MutableSystemInterface (MSystem body obj) obj      d where
                                        add = mAdd mArtificial
                                        rm  = mRm mArtificial
                                        upd = mUpdState mLookupArtificialEntry
                                        updBody     = meUpd mLookupArtificialEntry mPtr
                                        updImpulse  = meUpd mLookupArtificialEntry mImpulse
                                        updPosition = meUpd mLookupArtificialEntry mPosition

instance (Body body d, Body obj d) => MutableSystem          (MSystem body obj) body obj d where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--class (MutableSystem sys body obj d) => SystemMExec sys body obj d where
--    guardStellarInteraction :: sys d -> body d -> BodyState d -> IO()

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--instance SystemExecCache (MSystem body obj) d where
--instance (BasicSystem sys StellarBodyContainer ArtificialContainer d) => SystemExecCache sys d where

--instance SystemExecCache (MSystem StellarBodyContainer ArtificialContainer) d where
--    systemStellarState sys sbc = get sys sbc
--    systemStellarStates sys = do --x <- ((U.Exec.Mutable.all sys) :: IO [body d])
--                                 let q = do x <- U.Exec.Mutable.all sys
--                                            return undefined
--                                 let q = do x <- ((U.Exec.Mutable.all (sys ::  MSystem StellarBodyContainer ArtificialContainer d)) :: IO [body d])
--                                            let y = x :: [body d]
--                                            return undefined
--                                 []

--instance (BasicSystem sys StellarBodyContainer ArtificialContainer d) => SystemExecCache sys d where
--    systemStellarStates sys = do x <- U.Exec.Mutable.all sys
--                                 return x
--    systemStellarStates sys = join (U.Exec.Mutable.all sys)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


