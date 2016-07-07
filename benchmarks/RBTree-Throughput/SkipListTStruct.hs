{-|
STM concurrent Skip-list using TStruct.
Author: Ryan Yates
Based on Concurrent Priority Queue, copyright (c) Alex Semin, 2015.
License: BSD3
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module SkipListTStruct(
    SkipList,
    new,
    insert,
    get,
    delete
) where

import Data.Array.MArray
import Control.Monad
import Control.Applicative
import Control.Concurrent.STM
import GHC.Conc(unsafeIOToSTM)
import qualified System.Random.PCG.Fast.Pure as R
import qualified Data.Vector.Unboxed.Mutable as U
import System.Random.PCG.Class (sysRandom)
import Data.Word (Word64, Word32)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Concurrent

import SkipListNode

data SkipList = SkipList
  { _getHeadNodes :: Node
  , _getHeight    :: Int
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
  headNodes <- newNodeP height
  let states = unsafeDupablePerformIO $ do
        cn <- getNumCapabilities
        statev <- U.new (cn * cacheFactor)
        forM_ [0..cn-1] $ \i -> do
            seed <- sysRandom
            U.write statev (i * cacheFactor) seed
        return statev
  return $ SkipList headNodes height states

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
insert (SkipList headNodes height states) k v = do
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
        let !l = lvl-1
        next <- unsafeReadNode nodes l
        if isNil next
          then buildPrevs nodes l (nodes:prevs)
          else do
            -- Keys are immutable, so we can read non-transactionally.
            k' <- readKeyP next
            if k' == k
              then do
                v' <- readValue next
                when (v == v') $ writeValue next v -- Update value if it differs
                return Nothing
              else if k' > k
                then buildPrevs nodes l (next:prevs)
                else buildPrevs next lvl prevs

      insertNode nodesHeight prevs = do
        nodes <- mkNodeP k v nodesHeight
        let updatePtrs lvl _ 
                | lvl >= nodesHeight = return ()
            updatePtrs lvl (p:ps) = do
                nextNode <- unsafeReadNode p lvl
                unsafeWriteNode p lvl nodes
                unsafeWriteNodeP nodes lvl nextNode -- Non-transactional, the node is private.
                updatePtrs (lvl+1) ps
            updatePtrs _ [] = error "SkipList: main layout must be not lower than new one"

        updatePtrs 0 prevs

get :: SkipList -> Key -> STM (Maybe Value)
get (SkipList headNodes height states) k = do
    loop height headNodes
       where
        loop 0   _     = return Nothing
        loop lvl nodes = do
           let !l = lvl - 1
           next <- unsafeReadNode nodes l
           if isNil next
             then loop l nodes
             else do
               k' <- readKeyP next -- Non-transactional
               if k' > k
                 then loop l nodes
                 else if k' < k
                        then loop lvl next
                        else Just <$> readValue next    


delete :: SkipList -> Key -> STM Bool
delete (SkipList headNodes height states) k = do
    loop height headNodes
  where
    loop 0   _     = return False
    loop lvl nodes = do
      let !l = lvl - 1
      next <- unsafeReadNode nodes l
      if isNil next
        then loop l nodes
        else do
          k' <- readKeyP next -- Non-transactional
          if k' > k
            then loop l nodes
            else if k' < k
                   then loop lvl next
                   else swingPtrs lvl nodes
    swingPtrs 0   _     = return True
    swingPtrs lvl nodes = do
      let !l = lvl - 1
      next <- unsafeReadNode nodes l
      nextNext <- unsafeReadNode next l
      unsafeWriteNode nodes l nextNext
      swingPtrs l nodes
