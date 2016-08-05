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
    delete,
    validate
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
import Data.List (any)

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
insert sl@(SkipList headNodes height states) k v = do
  r <- do
      mp <- buildPrevs headNodes height []
      case mp of
        Nothing    -> return ()
        Just prevs -> do
          cn <- unsafeIOToSTM $ do
                tid <- myThreadId
                fst `fmap` threadCapability tid
          let lvl = chooseLvl states cn height
          insertNode lvl prevs
  -- validate sl
  return r
    where
      -- Search for where to put the new key keeping a stack of
      -- nodes where we traversed down the way.  When we reach
      -- bottom, return the stack.
      buildPrevs _    0   prevs = return $ Just prevs
      buildPrevs node lvl prevs = do
        let !l = lvl-1
        -- We need to look at the next key so find the next
        -- node from our level index and current node.
        next <- unsafeReadNode node l
        if isNil next
          then buildPrevs node l (node:prevs) -- We have gone too far, go down a level.
          else do
            -- Keys are immutable, so we can read non-transactionally.
            k' <- readKeyP next
            if k' == k
              then do
                -- If we find the key already in the list, update the value if it is different
                v' <- readValue next
                when (v == v') $ writeValue next v -- Update value if it differs
                return Nothing
              else if k' > k
                then buildPrevs node l (node:prevs) -- We see a key passed the key we are inserting, go down.
                else buildPrevs next lvl prevs -- We are still before the key, go across.

      insertNode nodesHeight prevs = do
        node <- mkNodeP k v nodesHeight
        let updatePtrs lvl _ 
                | lvl >= nodesHeight = return ()
            updatePtrs lvl (p:ps) = do
                nextNode <- unsafeReadNode p lvl
                unsafeWriteNode p lvl node
                unsafeWriteNodeP node lvl nextNode -- Non-transactional, the node is private.
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
delete sl@(SkipList headNodes height states) k = do
    r <- loop height headNodes -- loop given height, 1+index into node
    -- validate sl
    return r
  where
    loop 0   _    = return False -- We hit bottom
    loop lvl node = do
      let !l = lvl - 1 -- index for the level

      -- This is safe as l is always less then height
      -- and greater then or equal zero.  The head
      -- node always has full height so we can always
      -- start there.
      next <- unsafeReadNode node l
      if isNil next
        then loop l node -- drop a level, we hit the end
        else do
          -- read the key of the next node.  This is safe to be a
          -- non-transactional read because keys never change. 
          k' <- readKeyP next
          if k' > k
            then loop l node -- k' is passed, drop a level
            else if k' < k
                then loop lvl next -- keep at the same level and search
                else unlink node next lvl -- keys are equal!  Unlink next.

    -- At this point we have found the node with the matching key.  To
    -- unlink we walk down the previous node and mutate its links to
    -- match the values inside the node we are unlinking.  If the link
    -- from the previous points to a node other then 'node', we follow
    -- the link until we get a pointer to node:
    --
    -- head
    -- X    prev  node
    -- X   /     /
    -- X  X     X
    -- X  X     X
    -- X  X  X  X  
    -- X  X  X  X  X
    -- 0  1  2  3  4
    --
    -- Searching for '3' here will find 'node' with 'prev' at
    -- level 3.  To unlink 'node', 'prev' at 3 and 2 will need
    -- to point to Nil and node 2 will have to point to Nil at
    -- level 1 and node 4 at level 0.
    --
    unlink prev' node lvl = go lvl prev'
      where 
        go 0   _    = return True -- done unlinking node
        go lvl prev = do
          let !l = lvl - 1
          prev'' <- findAndWrite l prev
          go l prev''

        findAndWrite l Nil  = error "Found Nil while unlinking."
        findAndWrite l prev = do
          n <- unsafeReadNode prev l
          if n == node
            then do
              next <- unsafeReadNode node l
              unsafeWriteNode prev l next
              return prev
            else findAndWrite l n

assert s d b = unless b $ error (s++"\n    "++d)

withTails [x] = []
withTails (x:xs) = (x,xs) : withTails xs

validate :: SkipList -> STM ()
validate (SkipList headNode height states) = do

    assert "The head node is exactly height."
          (show (levels headNode, height))
          $ (levels headNode == height)

    ns <- getLevel 0 headNode
    assert "No nodes are higher then height." 
          (show (map levels (headNode:ns), height))
          $ maximum (map levels (headNode:ns)) <= height

    let loop i ps | i >= height = return ()
                  | otherwise   = do
            ls <- getLevel i headNode
            forM_ ls $ \n -> do
                assert "Each level is a subset of its lower level." 
                      ""
                      $ any (== n) ps
    loop 0 ns

    forM_ (withTails (headNode:ns)) $ \(node, nodes) -> do
        forFirstNil node $ \i ->
            assert "if a node points to Nil at height i, all subsequent nodes have height < i"
                  (show (map levels nodes, i, map levels (headNode:ns)))
                  $ maximum (map levels nodes) <= i -- i is index so <=

    forM_ ns $ \node -> do
        forFirstNil node $ \i ->
            forM_ [i..levels node - 1] $ \j -> do
                n <- unsafeReadNode node j
                assert "All pointers are Nil above the first Nil."
                      (show (i,j))
                      $ isNil n
                
  where
    getLevel :: Int -> Node -> STM [Node]
    getLevel l node = do
        n <- unsafeReadNode node l
        if isNil n
          then return []
          else (n :) <$>  getLevel l n

    forFirstNil node act = do
        let l = levels node
            go i | i >= l    = return ()
                 | otherwise = do
              n <- unsafeReadNode node i
              if isNil n
                then act i
                else go (i+1)
        go 0



delete' :: SkipList -> Key -> STM Bool
delete' (SkipList headNodes height states) k = do
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
