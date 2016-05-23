{-# LANGUAGE MagicHash #-}
module HAMTTStruct where

import Prelude hiding (lookup)

import Numeric

import GHC.Num
import GHC.ST
import GHC.Base hiding (assert)
import GHC.Conc
import GHC.Word
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)

import Data.List (nub)

-- import Prelude (Maybe(..), pred, succ, (=<<), ($!), snd, unlines)

import qualified STMContainers.WordArray.Indices as Indices
import qualified STMContainers.HAMT.Level as Level

import Data.Hashable
import Control.Monad (forM_, forM)
import Control.Applicative ((<$>))

-- In original HAMT the Node type has three constructors:
--
--   data Node = Nodes (WordArray (Node e))
--             | Leaf Hash e
--             | Leaves Hash (SizedArray e)
--
-- If we collapse leaf and leaves:
--
--   data Node = Nodes (WordArray (Node e))
--             | Leaves Hash (SizedArray e)
--
-- Now represent both of these with an STMMutableArray# version of the following:
--
--   data Nodes  = Nodes Indices [Nodes e]
--   data Leaves = Leaves Hash   [e]
--
-- In the monomorphic unboxed case Leaves stores everything in words while Nodes
-- always has pointers.  We can use a word for the tag bit and represent both with
-- a STMMutableArray#.  The tag bit would be read-only so we can cheat in STM
-- and do a non-transactional read:
--
--   struct Nodes
--   {
--      halfword sizeWords;
--      halfword sizePtrs;
--
--      word   tag;
--      word   indices;
--      Nodes* nodes[];
--   }
--
--   struct Leaves
--   {
--      halfword sizeWords;
--      halfword sizePtrs;
--
--      word tag;
--      word Hash;
--      word words[];
--   }
--
-- We will assume word sized keys and heap object values are being stored (to match the
-- original).
--

newtype Map k v = Map { unMap :: TVar (WordArray (k,v)) }

type Key a = (Eq a, Hashable a)
type Hash = Int

class (Eq (ElementKey e)) => Element e where
  type ElementKey e
  elementKey :: e -> ElementKey e

instance (Eq k) => Element (k, v) where
  type ElementKey (k, v) = k
  elementKey (k, v) = k

data WordArray e = WordArray { unWordArray :: STMMutableArray# RealWorld e }

instance Eq (WordArray e) where
  (WordArray a) == (WordArray a') =
      case sameSTMMutableArray# a a' of
          0# -> False
          _  -> True

type Index = Int
type Indices = Int

-- Private API for building a new private WordArray that we can then use transactionally.
{-# INLINE newWordArrayP #-}
newWordArrayP :: Int -> STM (WordArray e)
newWordArrayP (I# i#) = STM $ \s1# ->
    case newSTMArray# i# 1# undefined s1# of
        (# s2#, marr# #) -> (# s2#, WordArray marr# #)

{-# INLINE newWordArrayIO #-}
newWordArrayIO :: Int -> IO (WordArray e)
newWordArrayIO (I# i#) = IO $ \s1# ->
    case newSTMArray# i# 1# undefined s1# of
        (# s2#, marr# #) -> (# s2#, WordArray marr# #)

{-# INLINE newWordArrayInitP #-}
newWordArrayInitP :: Int -> e -> STM (WordArray e)
newWordArrayInitP (I# i#) e = STM $ \s1# ->
    case newSTMArray# i# 1# e s1# of
        (# s2#, marr# #) -> (# s2#, WordArray marr# #)

{-# INLINE readTagP #-}
readTagP :: WordArray e -> STM Word
readTagP (WordArray arr#) = STM $ \s1# ->
    case readSTMArrayWord# arr# 0# s1# of
        (# s2#, w# #) -> (# s2#, W# w# #)

---------------------------------------------------
-- Nodes
{-# INLINE roundUp64# #-}
roundUp64# :: Int# -> Int#
roundUp64# i# = (i# +# 63#) `andI#` (-64#)

{-# INLINE newNodesInitP #-}
newNodesInitP :: Int -> WordArray e -> STM (WordArray e)
newNodesInitP (I# i#) e = STM $ \s1# ->
    case newSTMArray# (roundUp64# i#) 2# (unsafeCoerce e) s1# of
--    case newSTMArray# i# 2# (unsafeCoerce e) s1# of
        (# s2#, arr# #) -> 
            case writeSTMArrayWord# arr# 0# 0## s2# of -- Set the tag to zero
                s3# -> (# s3#, WordArray arr# #)

{-# INLINE newNodesP #-}
newNodesP :: Int -> STM (WordArray e)
newNodesP (I# i#) = STM $ \s1# ->
    case newSTMArray# (roundUp64# i#) 2# undefined s1# of -- round up pointer count so words are on next cacheline
--    case newSTMArray# i# 2# undefined s1# of  <-- Minimal allocation
        (# s2#, arr# #) -> 
            case writeSTMArrayWord# arr# 0# 0## s2# of -- Set the tag to zero
                s3# -> (# s3#, WordArray arr# #)

{-# INLINE modifyIndicesP #-}
modifyIndicesP :: WordArray e -> (Int -> Int) -> STM ()
modifyIndicesP arr f = do
    i <- readIndicesP arr
    writeIndicesP arr (f i)

{-# INLINE readIndicesP #-}
readIndicesP :: WordArray e -> STM Int
readIndicesP (WordArray arr#) = STM $ \s1# ->
    case readSTMArrayWord# arr# 1# s1# of
        (# s2#, w# #) -> (# s2#, I# (word2Int# w#) #)

{-# INLINE writeIndicesP #-}
writeIndicesP :: WordArray e -> Int -> STM ()
writeIndicesP (WordArray arr#) (I# i#) = STM $ \s1# ->
    case writeSTMArrayWord# arr# 1# (int2Word# i#) s1# of
        s2# -> (# s2#, () #)

---------------------------------------------------
-- Leaves

{-# INLINE newLeavesP #-}
newLeavesP :: Int -> STM (WordArray e)
newLeavesP (I# i#) = STM $ \s1# ->
    case newSTMArray# i# 2# undefined s1# of
        (# s2#, arr# #) -> 
            case writeSTMArrayWord# arr# 0# 1## s2# of -- Set the tag to one
                s3# -> (# s3#, WordArray arr# #)

{-# INLINE modifyHashP #-}
modifyHashP :: WordArray e -> (Hash-> Hash) -> STM ()
modifyHashP arr f = do
    i <- readHashP arr
    writeHashP arr (f i)

{-# INLINE readHashP #-}
readHashP :: WordArray e -> STM Hash
readHashP (WordArray arr#) = STM $ \s1# ->
    case readSTMArrayWord# arr# 1# s1# of
        (# s2#, w# #) -> (# s2#, I# (word2Int# w#) #)

{-# INLINE writeHashP #-}
writeHashP :: WordArray e -> Hash -> STM ()
writeHashP (WordArray arr#) (I# i#) = STM $ \s1# ->
    case writeSTMArrayWord# arr# 1# (int2Word# i#) s1# of
        s2# -> (# s2#, () #)

---------------------------------------------------
-- Pointers
--

{-# INLINE dataSize #-}
dataSize :: WordArray e -> Int
dataSize (WordArray arr#) =
    case sizeofSTMMutableArray# arr# of
        i# -> I# i#

{-# INLINE readDataP #-}
readDataP :: WordArray e -> Int -> STM e
readDataP arr (I# i#) = STM $ \s1# -> readSTMArray# (unWordArray arr) i# s1#

{-# INLINE writeDataP #-}
writeDataP :: WordArray e -> Int -> e -> STM ()
writeDataP arr (I# i#) e = STM $ \s1# ->
    case writeSTMArray# (unWordArray arr) i# e s1# of
        s2# -> (# s2#, () #)

{-# INLINE writeNodesDataP #-}
writeNodesDataP :: WordArray e -> Int -> WordArray e -> STM ()
writeNodesDataP arr (I# i#) e = STM $ \s1# ->
    case writeSTMArray# (unWordArray arr) i# (unsafeCoerce e) s1# of
        s2# -> (# s2#, () #)

{-# INLINE readNodesData #-}
readNodesData :: WordArray e -> Int -> STM (WordArray e)
readNodesData a i = do
    e <- readData a i
    return (unsafeCoerce e)

{-# INLINE readData #-}
readData :: WordArray e -> Int -> STM e
readData arr (I# i#) = STM $ \s# -> readTArray# (unWordArray arr) (int2Word# i#) s#

{-# INLINE writeData #-}
writeData :: WordArray e -> Int -> e -> STM ()
writeData arr (I# i#) e = STM $ \s1# ->
    case writeTArray# (unWordArray arr) (int2Word# i#) e s1# of
        s2# -> (# s2#, () #)

{-# INLINE writeNodesData #-}
writeNodesData :: WordArray e -> Int -> WordArray e -> STM ()
writeNodesData arr (I# i#) e = STM $ \s1# ->
    case writeTArray# (unWordArray arr) (int2Word# i#) (unsafeCoerce e) s1# of
        s2# -> (# s2#, () #)

---------------------------------------------------
-- Operations
--

lookupLevel :: Element e => Hash -> ElementKey e -> Level.Level -> WordArray e -> STM (Maybe e)
lookupLevel h k l arr = do
    t <- readTagP arr
    case t of
        0 -> do -- A nodes level.
            let i = Level.hashIndex l h
            b <- readIndicesP arr -- TODO: if we can grow this must be transactional
            if Indices.elem i b
              then readNodesData arr (Indices.position i b) >>= lookupLevel h k (Level.succ l)
              else return Nothing
        1 -> do -- A leaf level.
            h' <- readHashP arr
            if h == h'
              then do
                let sz = dataSize arr
                    find i
                      | i >= sz   = return Nothing
                      | otherwise = do
                          e <- readData arr i
                          if elementKey e == k
                            then return $! Just e
                            else find (i+1)
                find 0
              else return Nothing

{-# INLINE set #-}
set :: Index -> WordArray e -> WordArray e -> STM (WordArray e)
set i e a = do
    b <- readIndicesP a
    let sparseIndex = Indices.position i b
        size = Indices.size b

    if Indices.elem i b
      then writeNodesData a sparseIndex e >> return a
      else do
        a' <- newNodesP (size+1)
        forM_ [0..sparseIndex-1] $ \i -> readNodesData a i >>= writeNodesDataP a' i
        writeNodesDataP a' sparseIndex e
        forM_ [sparseIndex..(size-1)] $ \i -> readNodesData a i >>= writeNodesDataP a' (i+1)
        writeIndicesP a' (Indices.insert i b)
        return a'

{-# NOINLINE deleteMarker #-}
deleteMarker :: WordArray e
deleteMarker = unsafePerformIO $ newWordArrayIO 0

{-# INLINE unset #-}
unset :: Index -> WordArray e -> STM (WordArray e)
unset i a = do
    -- TODO: handling a Nodes that goes to one element.
    b <- readIndicesP a
    if Indices.elem i b
      then do
        let b' = Indices.invert i b
            size = Indices.size b
            sparseIndex = Indices.position i b

        if size == 1
          then return deleteMarker
          else do
            a' <- newNodesP (pred size)
            forM_ [0..pred sparseIndex] $ \i -> readNodesData a i >>= writeNodesDataP a' i
            forM_ [succ sparseIndex..pred size]
                $ \i -> readNodesData a i >>= writeNodesDataP a' (pred i)
            writeIndicesP a' b'
            return a'
      else return a

{-# INLINE unset' #-}
unset' :: Index -> WordArray e -> STM (Maybe (WordArray e))
unset' i a = do
    -- TODO: handling a Nodes that goes to one element.
    b <- readIndicesP a
    if Indices.elem i b
      then do
        let b' = Indices.invert i b
            size = Indices.size b
            sparseIndex = Indices.position i b

        if size == 1
          then return Nothing
          else do
            a' <- newNodesP (pred size)
            forM_ [0..pred sparseIndex] $ \i -> readNodesData a i >>= writeNodesDataP a' i
            forM_ [succ sparseIndex..pred size]
                $ \i -> readNodesData a i >>= writeNodesDataP a' (pred i)
            writeIndicesP a' b'
            return (Just a')
      else return (Just a)

{-# INLINE singleton #-}
singleton :: Index -> WordArray e -> STM (WordArray e)
singleton i a = do
    ns <- newNodesInitP 1 a
    writeIndicesP ns (Indices.singleton i)
    return ns

{-# INLINE pairNodes #-}
pairNodes :: Index -> WordArray e -> Index -> WordArray e -> STM (WordArray e)
pairNodes i e i' e' = do
    if  | i < i' -> do
          a <- newNodesInitP 2 e
          writeNodesDataP a 1 e'
          writeIndicesP a is
          return a
        | i > i' -> do
          a <- newNodesInitP 2 e
          writeNodesDataP a 0 e'
          writeIndicesP a is
          return a
        | i == i' -> do
          a <- newNodesInitP 1 e'
          writeIndicesP a is
          return a
  where
    is = Indices.fromList [i, i']

pair :: Hash -> WordArray e -> Hash -> WordArray e -> Level.Level -> STM (WordArray e)
pair h1 n1 h2 n2 l =
  -- Even if the whole hashes do not match, the next chunk of it might, so check
  -- if the indexes at this level match.  If they do, make a new level and recurse
  -- until we find the difference.
 
  if i1 == i2
    then singleton i1 =<< pair h1 n1 h2 n2 (Level.succ l)
    else pairNodes i1 n1 i2 n2
  where
    hashIndex = Level.hashIndex l
    i1 = hashIndex h1
    i2 = hashIndex h2


insertLevel :: Element e => Hash -> e -> Level.Level -> WordArray e -> STM (WordArray e)
insertLevel h e l a = do
    let update i old new
          | old /= new = writeNodesData a i new
          | otherwise  = return ()
        k = elementKey e
    t <- readTagP a
    case t of
        0 -> do
            -- We have a nodes level
            let !i = Level.hashIndex l h
            b <- readIndicesP a
            if Indices.elem i b
                then do
                    -- If there is already an entry for the hash at this level, follow
                    -- it down.
                    let !sparseIndex = Indices.position i b
                    a' <- readNodesData a sparseIndex
                    insertLevel h e (Level.succ l) a' >>= update sparseIndex a'
                    return a
                else do
                    -- There is not an entry for the hash at this level:
                    --   - Make a new leaf entry 
                    ls <- newLeavesP 1
                    writeHashP ls h
                    writeDataP ls 0 e
                    --   - Expand this level to include a new entry
                    --     returning the new nodes level.
                    set i ls a
                    
        1 -> do
            -- We have a leaf level do the hashes match?
            h' <- readHashP a
            if h == h'
              then do
                -- Hashes match.  Check to see if the match is due to a
                -- matching key, if so, replace the value otherwise, expand to
                -- include this new entry as a collision.
                let !sz = dataSize a
                    find i
                      | i >= sz   = do
                            -- Expand and add the new value.
                            ls <- newLeavesP (sz+1)
                            writeHashP ls h
                            forM_ [0..sz-1] $ \j -> do
                                readData a j >>= writeDataP ls j
                            writeDataP ls sz e
                            return ls
                      | otherwise = do
                          e' <- readData a i
                          if elementKey e' == k
                            then do
                              -- TODO: Do not replace!  This is something
                              -- that could be specalized in a monomorphic 
                              -- version where value comparisons could happen.
                              -- -- Found entry with same key, replace.
                              -- writeData a i e
                              return a
                            else find (i+1)
                find 0
              else do
                -- Hashes do not match, we need to turn this into a nodes level
                -- with a pair of leaf levels below.
                --   - Make a new single leaf for the new entry.
                ls <- newLeavesP 1
                writeHashP ls h
                writeDataP ls 0 e

                --   - Make a new nodes level, Link the new leaf (ls), and the old leaves (a).
                --     Return this pair to replace the old level (may be a chain of new levels
                --     down to the difference in the hash).
                pair h' a h ls l

-- Delete from a level
--
--  Cases we must handle.
--
--  - Deleting form a collision leaf
--      
--      Shrink the leaf, updated parent pointer.
--
--  - Deleting from a singlton leaf when the parent nodes has multiple entries
--
--      Shrink the parent, update grandparent pointer.
--
--  - Deleting from a singlton leaf when the parent nodes is also a singlton
--
--      Shrink grandparent, update above.
--
--  Recursive call can dictate the following actions when it returns:
--
--      - Update pointer to new entry
--      - Shrink removing the entry
--          - Signal above to shrink
--
deleteLevel 
    :: (Element e) 
    => Hash 
    -> ElementKey e 
    -> Level.Level 
    -> WordArray e 
    -> STM (WordArray e)

deleteLevel h k l a = do
    let update i sparseIndex old new
          | new == deleteMarker = unset i a -- remove entry, shrink
          | old /= new          = writeNodesData a sparseIndex new >> return a
          | otherwise           = return a
    t <- readTagP a
    case t of
        0 -> do
            -- We have a nodes level
            let !i = Level.hashIndex l h
            b <- readIndicesP a
            if Indices.elem i b
                then do
                    -- If there is already an entry for the hash at this level, follow
                    -- it down.
                    let !sparseIndex = Indices.position i b
                    a' <- readNodesData a sparseIndex
                    deleteLevel h k (Level.succ l) a' >>= update i sparseIndex a'
                else -- There is not an entry for the hash at this level, we are done.
                    return a

        1 -> do
            -- We have a leaf level do the hashes match?
            h' <- readHashP a
            if h == h'
              then do
                -- Hashes match.  Check to see if the match is due to a
                -- matching key, if so shrink the leaf.
                let !sz = dataSize a
                    find i
                      | i >= sz   = return a -- Key not found, do nothing.
                      | otherwise = do
                          e' <- readData a i
                          if elementKey e' == k
                            then do
                              -- Found entry with same key, shrink.
                                if sz == 1
                                  then return deleteMarker
                                  else do
                                    ls <- newLeavesP (sz-1)
                                    writeHashP ls h
                                    forM_ [0..i-1] $ \j ->
                                        readData a j >>= writeDataP ls j
                                    forM_ [i+1..sz-1] $ \j ->
                                        readData a j >>= writeDataP ls (j-1)
                                    return $ ls
                            else find (i+1)
                find 0
              else return a -- Hashes do not match, we are done.

deleteLevel'
    :: (Element e) 
    => Hash 
    -> ElementKey e 
    -> Level.Level 
    -> WordArray e 
    -> STM (Maybe (WordArray e))

deleteLevel' h k l a = do
    let update i sparseIndex old Nothing = unset' i a -- remove entry, shrink
        update i sparseIndex old (Just new) -- Update pointer
          | old /= new = writeNodesData a sparseIndex new >> return (Just a)
          | otherwise  = return (Just a)
    t <- readTagP a
    case t of
        0 -> do
            -- We have a nodes level
            let i = Level.hashIndex l h
            b <- readIndicesP a
            if Indices.elem i b
                then do
                    -- If there is already an entry for the hash at this level, follow
                    -- it down.
                    let sparseIndex = Indices.position i b
                    a' <- readNodesData a sparseIndex
                    deleteLevel' h k (Level.succ l) a' >>= update i sparseIndex a'
                else -- There is not an entry for the hash at this level, we are done.
                    return (Just a)

        1 -> do
            -- We have a leaf level do the hashes match?
            h' <- readHashP a
            if h == h'
              then do
                -- Hashes match.  Check to see if the match is due to a
                -- matching key, if so shrink the leaf.
                let sz = dataSize a
                    find i
                      | i >= sz   = return (Just a) -- Key not found, do nothing.
                      | otherwise = do
                          e' <- readData a i
                          if elementKey e' == k
                            then do
                              -- Found entry with same key, shrink.
                                if sz == 1
                                  then return Nothing
                                  else do
                                    ls <- newLeavesP (sz-1)
                                    writeHashP ls h
                                    forM_ [0..i-1] $ \j ->
                                        readData a j >>= writeDataP ls j
                                    forM_ [i+1..sz-1] $ \j ->
                                        readData a j >>= writeDataP ls (j-1)
                                    return $ Just ls
                            else find (i+1)
                find 0
              else return (Just a) -- Hashes do not match, we are done.
             
---------------------------------------------------
-- API
--

-- lookup :: (Show k, Show v, Key k) => k -> Map k v -> STM (Maybe v)
lookup :: (Key k) => k -> Map k v -> STM (Maybe v)
lookup k (Map m) = do
    e <- readTVar m >>= lookupLevel (hash k) k 0
    return $ fmap snd e

-- insert :: (Show k, Show v, Key k) => k -> v -> Map k v -> STM ()
insert :: (Key k) => k -> v -> Map k v -> STM ()
insert k v (Map m) = do
    a <- readTVar m
    a' <- insertLevel (hash k) (k,v) 0 a
    if a == a'
      then return ()
      else writeTVar m a' -- >> validate (Map m)

-- delete :: (Show k, Show v, Key k) => k -> Map k v -> STM ()
delete :: (Key k) => k -> Map k v -> STM ()
delete k (Map m) = do
    a <- readTVar m
    a' <- deleteLevel (hash k) k 0 a
        
    if a' == deleteMarker
      then (newNodesP 0 >>= writeTVar m) --  >> validate (Map m)
      else if a == a'
          then return ()
          else writeTVar m a' -- >> validate (Map m)
{-
    assertM ("VVV Key still present after delete!") $ do
        lookup k (Map m) >>= \case
            Just _  -> return False
            Nothing -> return True -}

new :: STM (Map k v)
new = do
    n <- newNodesP 0
    v <- newTVar n 
    return (Map v)


showLevel :: (Show e, Element e) => WordArray e -> STM [String]
showLevel a = do
    t <- readTagP a
    case t of
      0 -> do
        -- Nodes
        b <- readIndicesP a
        let sz = Indices.size b
        ls <- forM [0..sz-1] $ \i -> readNodesData a i >>= showLevel
        return $ ("Nodes " ++ showBin b) : map ("  "++) (concat ls)
      1 -> do
        -- Leaf
        h <- readHashP a
        let sz = dataSize a
        ls <- forM [0..sz-1] $ \i -> show <$> readData a i
        return $ ("Leaf " ++ showBin h) : map ("  "++) ls
  where
    showBin x = showIntAtBase 2 ("01"!!) x ""

assertM :: Show e => e -> STM Bool -> STM ()
assertM m a = do
    b <- a
    if b
      then return ()
      else unsafeIOToSTM (print m)

assert :: Show e => e -> Bool -> STM ()
assert m b = do
    if b
      then return ()
      else unsafeIOToSTM (print m)

-- Find the first hash available.
findHash :: WordArray e -> STM Hash
findHash a = do
    t <- readTagP a
    case t of
      0 -> do
        -- Nodes
        assert ("VVV Empty non-root nodes") $ dataSize a > 0
        -- Recurse to find the first hash.  Really we want all
        -- hashes to match, but this aproximates.
        readNodesData a 0 >>= findHash
      1 -> readHashP a

validateLevel :: (Show e, Element e, Eq (ElementKey e), Key (ElementKey e)) => Level.Level -> WordArray e -> STM ()
validateLevel l a = do
    t <- readTagP a
    case t of
      0 -> do
        -- Nodes
        b <- readIndicesP a
        let sz = Indices.size b

        assert ("VVV PopCount dataSize mismatch", sz, dataSize a) $ sz == dataSize a

        forM_ [0..sz-1] $ \i -> do
            a' <- readNodesData a i
            h <- findHash a'

            let h' = Level.hashIndex l h
                i' = Indices.position h' b

            assert ("VVV First hash does not match.", i, i', h', h, b, l) $ i == i'

            validateLevel (Level.succ l) a'

      1 -> do
        -- leaf
        h <- readHashP a

        let sz = dataSize a
        assert ("VVV Empty Leaf!", h) $ sz > 0

        -- check that keys and hashes match, keys do not match.
        es <- forM [0..sz-1] $ readData a
        let ks = map elementKey es

        assert ("VVV Keys to not all hash same", es, map hash ks) $ nub (map hash ks) == [h]
        assert ("VVV Some keys match!", es) $ nub ks == ks
              

validate :: (Show k, Show v, Key k) => Map k v -> STM ()
validate (Map m) = readTVar m >>= validateLevel 0

showSTM :: (Show k, Show v, Key k) => Map k v -> STM String
showSTM (Map m) = do
    a <- readTVar m
    unlines <$> showLevel a
