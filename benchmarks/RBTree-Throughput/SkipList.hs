{-|
Skip-list implementation based on implementation copyright (c) Alex Semin, 2015.
License     : BSD3
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}
module SkipList(
    SkipList,
    new,
    insert,
    get,
    contains,
    delete
) where

import Data.Array.MArray
import Control.Monad
import Data.Word
import Data.Maybe
import Control.Applicative
#ifdef PASTMTL2
import Control.TL2.STM
#elif defined(NOREC)
import Control.NoRec.STM
#else
import Control.Concurrent.STM
import GHC.Conc(unsafeIOToSTM)
#endif
import qualified System.Random.PCG.Fast.Pure as R
import qualified Data.Vector.Unboxed.Mutable as U
import System.Random.PCG.Class (sysRandom)
import Data.Word (Word64, Word32)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Concurrent

#if defined(PASTMTL2) || defined(NOREC)
import Data.Array (Array, bounds)
import Data.Array.Base (listArray, arrEleBottom, unsafeAt, MArray(..),
                        IArray(numElements))
import Data.Ix (rangeSize)
import Data.Typeable (Typeable)
import GHC.Base(IO(..))
newtype TArray i e = TArray (Array i (TVar e)) deriving (Eq, Typeable)

instance MArray TArray e STM where
    getBounds (TArray a) = return (bounds a)
    newArray b e = do
        a <- rep (rangeSize b) (newTVar e)
        return $ TArray (listArray b a)
    newArray_ b = do
        a <- rep (rangeSize b) (newTVar arrEleBottom)
        return $ TArray (listArray b a)
    unsafeRead (TArray a) i = readTVar $ unsafeAt a i
    unsafeWrite (TArray a) i e = writeTVar (unsafeAt a i) e
    getNumElements (TArray a) = return (numElements a)
-- | Like 'replicateM' but uses an accumulator to prevent stack overflows.
-- Unlike 'replicateM' the returned list is in reversed order.
-- This doesn't matter though since this function is only used to create
-- arrays with identical elements.
rep :: Monad m => Int -> m a -> m [a]
rep n m = go n []
    where
      go 0 xs = return xs
      go i xs = do
          x <- m
          go (i-1) (x:xs)
          
unsafeIOToSTM (IO a) = STM a
#endif

type Key = Word
type Value = Word

type Nodes = TArray Int Node

data Node = Nil
          | Node
          { _getKey   :: Key
          , _getVal   :: TVar Value
          , _getNodes :: Nodes
          }

-- | Abbreviation stands for Per Thread Seed TArray Skip-List Priority Queue
data SkipList = SkipList
  { _getHeadNodes :: Nodes
  , _getHeight    :: TVar Int
  , _getStates    :: U.IOVector Word64
  }

-- | Constant for aligning RNG seed to cache-line
-- which usually is 64 Kb long (while seed only is 'Data.Word64').
cacheFactor :: Int
cacheFactor = 8

-- | Parameterizing constructor which determines
-- maximum height of skip-list node.
new :: Int -> STM SkipList
new height = do
  headNodes <- newArray (1, height) Nil
  vHeight <- newTVar height
  let states = unsafeDupablePerformIO $ do
        cn <- getNumCapabilities
        statev <- U.new (cn * cacheFactor)
        forM_ [0..cn-1] $ \i -> do
            seed <- sysRandom
            U.write statev (i * cacheFactor) seed
        return statev
  return $ SkipList headNodes vHeight states

mbw32f :: Float
mbw32f = fromIntegral (maxBound :: Word32)

logHalf :: Float
logHalf = log 0.5

-- Obtains PCG state, generate random value and store new state
gen :: U.IOVector Word64 -> Int -> Word32
gen v !i = unsafeDupablePerformIO $ do
  let i' = i * cacheFactor
  st <- U.read v i'
  let (R.P st' x) = R.pair st
  U.write v i' st'
  return x

chooseLvl :: U.IOVector Word64 -> Int -> Int -> Int
chooseLvl v !i !h = min h $ (+1) $ truncate $ log x / logHalf
    where x = fromIntegral (gen v i) / mbw32f

insert :: SkipList -> Key -> Value -> STM ()
insert (SkipList headNodes vHeight states) k v = do
  height <- readTVar vHeight
  mp <- buildPrevs headNodes height []
  case mp of
    Nothing    -> return ()
    Just prevs -> do
      cn <- unsafeIOToSTM $ do
        tid <- myThreadId
        fst `fmap` threadCapability tid
      let lvl = chooseLvl states cn height
      insertNode lvl prevs
    where
      buildPrevs _     0   prevs = return $ Just prevs
      buildPrevs nodes lvl prevs = do
        next <- readArray nodes lvl
        case next of
          Nil -> buildPrevs nodes (lvl-1) (nodes:prevs)
          (Node k' tv nodes') ->
            if k' == k
              then do
                v' <- readTVar tv
                when (v' == v) $ writeTVar tv v
                return Nothing
              else if k' > k
                then buildPrevs nodes (lvl-1) (nodes:prevs)
                else buildPrevs nodes' lvl prevs

      insertNode nodesHeight prevs = do
        nodes <- newArray_ (1, nodesHeight)
        vv <- newTVar v
        let newNode = Node k vv nodes
            updatePtrs lvl _ | lvl > nodesHeight = return ()
            updatePtrs lvl (p:ps) = do
                nextNode <- readArray p lvl
                writeArray p lvl newNode
                writeArray nodes lvl nextNode
                updatePtrs (lvl+1) ps
            updatePtrs _ [] = error "SkipList: main layout must be not lower than new one"

        updatePtrs 1 prevs

get :: SkipList -> Key -> STM (Maybe Value)
get (SkipList headNodes vHeight states) k = do
    height <- readTVar vHeight
    lp height headNodes
       where
        lp 0 nodes = return Nothing
        lp lvl nodes = do
           next <- readArray nodes lvl
           case next of
                Nil -> lp (lvl-1) nodes
                Node k' v nodes' ->
                     if k' > k
                     then lp (lvl - 1) nodes
                     else if k' < k
                          then lp lvl nodes'
                          else Just <$> readTVar v

contains :: SkipList -> Key -> STM Bool
contains t k = isJust <$> get t k

delete :: SkipList -> Key -> STM Bool
delete (SkipList headNodes vHeight states)  k = do
       height <- readTVar vHeight
       lp height headNodes
       where
        lp 0 nodes = return False
        lp lvl nodes = do
           next <- readArray nodes lvl
           case next of
                Nil -> lp (lvl-1) nodes
                Node k' _ nodes' ->
                     if k' > k
                     then lp (lvl - 1) nodes
                     else if k' < k
                          then lp lvl nodes'
                          else swingPtrs lvl nodes
        swingPtrs 0 nodes = return True
        swingPtrs lvl nodes = do
                  next <- readArray nodes lvl
                  nextNext <- readArray (_getNodes next) lvl
                  writeArray nodes lvl nextNext
                  swingPtrs (lvl-1) nodes
