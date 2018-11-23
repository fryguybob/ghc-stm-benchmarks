{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE MutableFields #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE UnboxedTuples #-}
module HamtTRef where

import Prelude hiding (lookup)

import Numeric

import GHC.Num
import GHC.ST
import GHC.Base hiding (assert)
import GHC.Conc
import GHC.Word
import GHC.Prim

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

newtype Map k v = Map { unMap :: TVar (Node (k,v)) }

type Key a = (Eq a, Hashable a)
type Hash = Int

class (Eq (ElementKey e)) => Element e where
  type ElementKey e
  elementKey :: e -> ElementKey e

instance (Eq k) => Element (k, v) where
  type ElementKey (k, v) = k
  elementKey (k, v) = k

data Node e where
-- TODO: fix this.  Right now the wrapper gets the type  Int# -> Int# -> ...
  Nodes   :: {-# UNPACK #-} !Indices -> mutableArray (Node e) -> STM (Node e)
  Leaves  :: {-# UNPACK #-} !Hash    -> mutableArray e        -> STM (Node e)
--  Nodes   :: Indices -> mutableArray (Node e) -> STM (Node e)
--  Leaves  :: Hash    -> mutableArray e        -> STM (Node e)
  Deleted :: Node e

instance Eq (Node e) where
  Deleted == Deleted = True
  _       == Deleted = False
  Deleted == _       = False
  a == a' =
      case sameSTMMutableArray# (unsafeCoerce# a) (unsafeCoerce# a') of
          0# -> False
          _  -> True

type Index = Int
type Indices = Int

---------------------------------------------------
-- Array operations

{-# INLINE refArraySize #-}
refArraySize :: RefArray# RealWorld e -> Int
refArraySize r =
    case refArraySize# r of
        i# -> I# i#

type DataArray e = RefArray# RealWorld e
type NodeArray e = RefArray# RealWorld (Node e)

{-# INLINE readDataP #-}
readDataP :: DataArray e -> Int -> STM e
readDataP arr (I# i#) = STM $ \s1# -> readRefArrayExt# arr i# s1#

{-# INLINE writeDataP #-}
writeDataP :: DataArray e -> Int -> e -> STM ()
writeDataP arr (I# i#) e = STM $ \s1# ->
    case writeRefArrayExt# arr i# e s1# of
        s2# -> (# s2#, () #)

{-# INLINE writeNodesDataP #-}
writeNodesDataP :: NodeArray e -> Int -> Node e -> STM ()
writeNodesDataP arr (I# i#) e = STM $ \s1# ->
    case writeRefArrayExt# arr i# e s1# of
        s2# -> (# s2#, () #)

{-# INLINE readNodesData #-}
readNodesData :: NodeArray e -> Int -> STM (Node e)
readNodesData arr (I# i#) = STM $ \s1# -> readTRefArray# arr i# s1#

{-# INLINE readData #-}
readData :: DataArray e -> Int -> STM e
readData arr (I# i#) = STM $ \s# -> readTRefArray# arr i# s#

{-# INLINE writeData #-}
writeData :: DataArray e -> Int -> e -> STM ()
writeData arr (I# i#) e = STM $ \s1# ->
    case writeTRefArray# arr i# e s1# of
        s2# -> (# s2#, () #)

{-# INLINE writeNodesData #-}
writeNodesData :: NodeArray e -> Int -> Node e -> STM ()
writeNodesData arr (I# i#) e = STM $ \s1# ->
    case writeTRefArray# arr i# e s1# of
        s2# -> (# s2#, () #)

{-# INLINE mkNodes #-}
mkNodes :: Indices -> Int -> STM (Node e)
mkNodes i (I# sz#) = Nodes i sz#

{-# INLINE mkLeaves #-}
mkLeaves :: Hash -> Int -> STM (Node e)
mkLeaves h (I# sz#) = Leaves h sz#

---------------------------------------------------
-- Operations
--

lookupLevel :: Element e => Hash -> ElementKey e -> Level.Level -> Node e -> STM (Maybe e)
lookupLevel h k l (Nodes b ns) = do
    let i = Level.hashIndex l h
    if Indices.elem i b
      then readNodesData ns (Indices.position i b) >>= lookupLevel h k (Level.succ l)
      else return Nothing
lookupLevel h k l (Leaves h' vs) 
    | h == h' = do
        let sz = refArraySize vs
            find i
              | i >= sz   = return Nothing
              | otherwise = do
                  e <- readData vs i
                  if elementKey e == k
                    then return $! Just e
                    else find (i+1)
        find 0
    | otherwise = return Nothing
lookupLevel _ _ _ Deleted = return Nothing

{-# INLINE set #-}
set :: Index -> Node e -> Node e -> STM (Node e)
set i e a@(Nodes b ns) = do
    let sparseIndex = Indices.position i b
        size = Indices.size b

    if Indices.elem i b
      then writeNodesData ns sparseIndex e >> return a
      else do
        a'@(Nodes _ ns') <- mkNodes (Indices.insert i b) (size+1)
        forM_ [0..sparseIndex-1] $ \i -> readNodesData ns i >>= writeNodesDataP ns' i
        writeNodesDataP ns' sparseIndex e
        forM_ [sparseIndex..(size-1)] $ \i -> readNodesData ns i >>= writeNodesDataP ns' (i+1)
        return a'
set _ _ _ = error "set on non Nodes"

{-# INLINE unset #-}
unset :: Index -> Node e -> STM (Node e)
unset i a@(Nodes b ns) = do
    -- TODO: handling a Nodes that goes to one element.
    if Indices.elem i b
      then do
        let b' = Indices.invert i b
            size = Indices.size b
            sparseIndex = Indices.position i b

        if size == 1
          then return Deleted
          else do
            a'@(Nodes _ ns') <- mkNodes b' (pred size)
            forM_ [0..pred sparseIndex] $ \i -> readNodesData ns i >>= writeNodesDataP ns' i
            forM_ [succ sparseIndex..pred size]
                $ \i -> readNodesData ns i >>= writeNodesDataP ns' (pred i)
            return a'
      else return a
unset _ _ = error "unset on non Nodes"

{-# INLINE unset' #-}
unset' :: Index -> Node e -> STM (Maybe (Node e))
unset' i a@(Nodes b ns) = do
    -- TODO: handling a Nodes that goes to one element.
    if Indices.elem i b
      then do
        let b' = Indices.invert i b
            size = Indices.size b
            sparseIndex = Indices.position i b

        if size == 1
          then return Nothing
          else do
            a'@(Nodes _ ns') <- mkNodes b' (pred size)
            forM_ [0..pred sparseIndex] $ \i -> readNodesData ns i >>= writeNodesDataP ns' i
            forM_ [succ sparseIndex..pred size]
                $ \i -> readNodesData ns i >>= writeNodesDataP ns' (pred i)
            return (Just a')
      else return (Just a)
unset' _ _ = error "unset' on non Nodes"


{-# INLINE singleton #-}
singleton :: Index -> Node e -> STM (Node e)
singleton i e = do
    a@(Nodes _ ns) <- mkNodes (Indices.singleton i) 1
    writeNodesDataP ns 0 e
    return a

{-# INLINE pairNodes #-}
pairNodes :: Index -> Node e -> Index -> Node e -> STM (Node e)
pairNodes i e i' e' = do
    if  | i < i' -> do
          a@(Nodes _ ns) <- mkNodes is 2
          writeNodesDataP ns 0 e
          writeNodesDataP ns 1 e'
          return a
        | i > i' -> do
          a@(Nodes _ ns) <- mkNodes is 2
          writeNodesDataP ns 0 e'
          writeNodesDataP ns 1 e
          return a
        | otherwise -> singleton is e'
  where
    is = Indices.fromList [i, i']

pair :: Hash -> Node e -> Hash -> Node e -> Level.Level -> STM (Node e)
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


insertLevel :: Element e => Hash -> e -> Level.Level -> Node e -> STM (Node e)
insertLevel h e l a@(Nodes b ns) 
    | Indices.elem i b = do
        -- If there is already an entry for the hash at this level, follow
        -- it down.
        let !sparseIndex = Indices.position i b
        a' <- readNodesData ns sparseIndex
        insertLevel h e (Level.succ l) a' >>= update sparseIndex a'
        return a
    | otherwise = do
        -- There is not an entry for the hash at this level:
        --   - Make a new leaf entry 
        ls@(Leaves _ vs) <- mkLeaves h 1
        writeDataP vs 0 e
        --   - Expand this level to include a new entry
        --     returning the new nodes level.
        set i ls a
  where
    update i old new
      | old /= new = writeNodesData ns i new
      | otherwise  = return ()
    !i = Level.hashIndex l h

insertLevel h e l a@(Leaves h' vs)
    | h == h' = do
        -- Hashes match.  Check to see if the match is due to a
        -- matching key, if so, replace the value otherwise, expand to
        -- include this new entry as a collision.
        let !sz = refArraySize vs
            k = elementKey e
            find i
              | i >= sz   = do
                  -- Expand and add the new value.
                  ls@(Leaves _ vs') <- mkLeaves h (sz+1)
                  forM_ [0..sz-1] $ \j -> do
                      readData vs j >>= writeDataP vs' j
                  writeDataP vs' sz e
                  return ls
              | otherwise = do
                  e' <- readData vs i
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
    | otherwise = do
         -- Hashes do not match, we need to turn this into a nodes level
         -- with a pair of leaf levels below.
         --   - Make a new single leaf for the new entry.
         ls@(Leaves _ vs) <- mkLeaves h 1
         writeDataP vs 0 e

         --   - Make a new nodes level, Link the new leaf (ls), and the old leaves (a).
         --     Return this pair to replace the old level (may be a chain of new levels
         --     down to the difference in the hash).
         pair h' a h ls l

insertLevel h e _ Deleted = do
    ls@(Leaves _ vs) <- mkLeaves h 1
    writeDataP vs 0 e
    return ls

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
    -> Node e
    -> STM (Node e)

deleteLevel h k l a@(Nodes b ns)
    | Indices.elem i b = do
         -- If there is already an entry for the hash at this level, follow
         -- it down.
         let !sparseIndex = Indices.position i b
         a' <- readNodesData ns sparseIndex
         deleteLevel h k (Level.succ l) a' >>= update i sparseIndex a'
    | otherwise = return a
  where
    !i = Level.hashIndex l h
    update i _ _ Deleted = unset i a
    update i sparseIndex old new
      | old /= new     = writeNodesData ns sparseIndex new >> return a
      | otherwise      = return a

deleteLevel h k l a@(Leaves h' vs)
    | h == h' = do
         -- Hashes match.  Check to see if the match is due to a
         -- matching key, if so shrink the leaf.
         let !sz = refArraySize vs
             find i
               | i >= sz   = return a -- Key not found, do nothing.
               | otherwise = do
                   e' <- readData vs i
                   if elementKey e' == k
                     then do
                       -- Found entry with same key, shrink.
                         if sz == 1
                           then return Deleted
                           else do
                             ls@(Leaves _ vs') <- mkLeaves h (sz-1)
                             forM_ [0..i-1] $ \j ->
                                 readData vs j >>= writeDataP vs' j
                             forM_ [i+1..sz-1] $ \j ->
                                 readData vs j >>= writeDataP vs' (j-1)
                             return $ ls
                     else find (i+1)
         find 0
    | otherwise = return a -- Hashes do not match, we are done.

deleteLevel _ _ _ Deleted = return Deleted

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
      then return () -- >> validate (Map m)
      else writeTVar m a' -- >> validate (Map m)
{-
    assertM ("VVV Key not present after insert!") $ do
        lookup k (Map m) >>= \case
            Just _  -> return True
            Nothing -> return False
-}

-- delete :: (Show k, Show v, Key k) => k -> Map k v -> STM ()
delete :: (Key k) => k -> Map k v -> STM ()
delete k (Map m) = do
    a <- readTVar m
    a' <- deleteLevel (hash k) k 0 a
        
    if a == a'
      then return () -- >> validate (Map m)
      else writeTVar m a' -- >> validate (Map m)
{-
    assertM ("VVV Key still present after delete!") $ do
        lookup k (Map m) >>= \case
            Just _  -> return False
            Nothing -> return True
-}

new :: STM (Map k v)
new = do
    v <- newTVar Deleted 
    return (Map v)


showLevel :: (Show e, Element e) => Node e -> STM [String]
showLevel (Nodes b ns) = do
    let sz = Indices.size b
    ls <- forM [0..sz-1] $ \i -> readNodesData ns i >>= showLevel
    return $ ("Nodes " ++ showBin b) : map ("  "++) (concat ls)
showLevel (Leaves h vs) = do
    let sz = refArraySize vs
    ls <- forM [0..sz-1] $ \i -> show <$> readData vs i
    return $ ("Leaf " ++ showBin h) : map ("  "++) ls
showLevel Deleted = return ["Deleted"]

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
findHash :: Node e -> STM Hash
findHash (Nodes _ ns) = do
        assert ("VVV Empty non-root nodes") $ refArraySize ns > 0
        -- Recurse to find the first hash.  Really we want all
        -- hashes to match, but this aproximates.
        readNodesData ns 0 >>= findHash
findHash (Leaves h _) = return h
findHash _ = error "Deleted in findHash"

validateLevel :: (Show e, Element e, Eq (ElementKey e), Key (ElementKey e)) => Level.Level -> Node e -> STM ()
validateLevel l a@(Nodes b ns) = do
    let sz = Indices.size b
    forM_ [0..sz-1] $ \i -> do
        a' <- readNodesData ns i
        h <- findHash a'

        let h' = Level.hashIndex l h
            i' = Indices.position h' b

        assert ("VVV First hash does not match.", i, i', h', h, b, l) $ i == i'

        validateLevel (Level.succ l) a'
validateLevel l a@(Leaves h vs) = do
    let sz = refArraySize vs
    assert ("VVV Empty Leaf!", h) $ sz > 0

    -- check that keys and hashes match, keys do not match.
    es <- forM [0..sz-1] $ readData vs
    let ks = map elementKey es

    assert ("VVV Keys to not all hash same", es, map hash ks) $ nub (map hash ks) == [h]
    assert ("VVV Some keys match!", es) $ nub ks == ks
              

validate :: (Show k, Show v, Key k) => Map k v -> STM ()
validate (Map m) = readTVar m >>= validateLevel 0

showSTM :: (Show k, Show v, Key k) => Map k v -> STM String
showSTM (Map m) = do
    a <- readTVar m
    unlines <$> showLevel a
