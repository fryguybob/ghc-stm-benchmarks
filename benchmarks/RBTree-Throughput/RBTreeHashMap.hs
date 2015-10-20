{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module RBTreeHashMap
    ( AtomicTree 
    , mkAtomicTree
    , Key
    
    , insert
    , delete
    , get
    ) where

import qualified Data.HashMap.Strict as M
import Control.Applicative
import Data.IORef
import Data.Word

#ifdef CAS
import Data.Atomics
#elif defined(TVARBOX)
import Control.Concurrent.STM
#elif defined(TMVARBOX)
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
#elif defined(MVARBOX)
import Control.Concurrent.MVar
#endif

type Key = Word

#ifdef TVARBOX
newtype AtomicTree v = AtomicTree { runAtomicTree :: TVar (M.HashMap Word v) }
#elif defined(TMVARBOX)
newtype AtomicTree v = AtomicTree { runAtomicTree :: TMVar (M.HashMap Word v) }
#elif defined(MVARBOX)
newtype AtomicTree v = AtomicTree { runAtomicTree :: MVar (M.HashMap Word v) }
#else
newtype AtomicTree v = AtomicTree { runAtomicTree :: IORef (M.HashMap Word v) }
#endif

#ifdef CAS
mkAtomicTree :: IO (AtomicTree v)
mkAtomicTree = AtomicTree <$> newIORef M.empty

get :: AtomicTree v -> Key -> IO (Maybe v)
get (AtomicTree p) k = M.lookup k <$> readIORef p

insert :: AtomicTree v -> Key -> v -> IO ()
insert (AtomicTree p) k v = atomicModifyIORefCAS_ p (M.insert k v)

delete :: AtomicTree v -> Key -> IO ()
delete (AtomicTree p) k = atomicModifyIORefCAS_ p (M.delete k)

-------------------------------------
#elif defined(TVARBOX)

mkAtomicTree :: IO (AtomicTree v)
mkAtomicTree = AtomicTree <$> newTVarIO M.empty

get :: AtomicTree v -> Key -> IO (Maybe v)
get (AtomicTree p) k = M.lookup k <$> readTVarIO p

insert :: AtomicTree v -> Key -> v -> IO ()
insert (AtomicTree p) k v = atomically $ modifyTVar p (M.insert k v)

delete :: AtomicTree v -> Key -> IO ()
delete (AtomicTree p) k = atomically $ modifyTVar p (M.delete k)

-------------------------------------
#elif defined(TMVARBOX)

mkAtomicTree :: IO (AtomicTree v)
mkAtomicTree = AtomicTree <$> newTMVarIO M.empty

get :: AtomicTree v -> Key -> IO (Maybe v)
get (AtomicTree p) k = atomically $ (M.lookup k) <$> readTMVar p

modifyTMVar p f = do
    x <- takeTMVar p
    putTMVar p (f x)

insert :: AtomicTree v -> Key -> v -> IO ()
insert (AtomicTree p) k v = atomically $ modifyTMVar p (M.insert k v)

delete :: AtomicTree v -> Key -> IO ()
delete (AtomicTree p) k = atomically $ modifyTMVar p (M.delete k)

------------------------------------
#elif defined(MVARBOX)

mkAtomicTree :: IO (AtomicTree v)
mkAtomicTree = AtomicTree <$> newMVar M.empty

get :: AtomicTree v -> Key -> IO (Maybe v)
get (AtomicTree p) k = M.lookup k <$> readMVar p

insert :: AtomicTree v -> Key -> v -> IO ()
insert (AtomicTree p) k v = modifyMVar p (\m -> return (M.insert k v m,()))

delete :: AtomicTree v -> Key -> IO ()
delete (AtomicTree p) k = modifyMVar p (\m -> return (M.delete k m, ()))

-------------------------------------
#else

mkAtomicTree :: IO (AtomicTree v)
mkAtomicTree = AtomicTree <$> newIORef M.empty

get :: AtomicTree v -> Key -> IO (Maybe v)
get (AtomicTree p) k = M.lookup k <$> readIORef p

insert :: AtomicTree v -> Key -> v -> IO ()
insert (AtomicTree p) k v = atomicModifyIORef' p (\m -> (M.insert k v m,()))

delete :: AtomicTree v -> Key -> IO ()
delete (AtomicTree p) k = atomicModifyIORef' p (\m -> (M.delete k m, ()))
#endif



