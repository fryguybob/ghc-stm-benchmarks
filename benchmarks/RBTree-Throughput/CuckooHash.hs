{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE MultiWayIf   #-}
module CuckooHash
    ( Table
    , mkTable

    , insert
    , remove
    , find
    
    , verify
    ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad

import Data.Word

import GHC.Conc
import GHC.Prim
import GHC.Stack
import GHC.Word
import GHC.Int
import GHC.Base hiding (assert)
import GHC.ST

import TStruct

-- #define CAPACITY    7
-- #define ROUND_LIMIT 8  -- CPP vs MagicHash!

-- Alternatively we can specalize this to some fixed representation of 
-- key and value and store it all in words.
newtype Bucket k v = Bucket { _unBucket :: TStruct (k, v) }
    deriving (Eq)

data BucketArray k v = BucketArray { _unBucketArray :: Array# Any }

data MutableBucketArray k v =
    MutableBucketArray { _unMutableBucketArray :: MutableArray# RealWorld Any }

-- Data structure.
--   We are attempting to remove some indirection here but the basic structure is 
--   a top level pair of arrays containing mutable buckets.
--
--   The top level pair is implemented as a TStruct (TODO: stuff size in there too)
--   and the arrays it contains are: Array# (STMMutableArray# RealWorld (k v)).
--   With sufficient unsafeCoerce# we can achieve this.  Once we find the bucket we 
--   want to work with we wrap it in a TStruct constructor (making it a Bucket) to 
--   make the code more manageable with the hope that inlining will let allow those
--   constructors to be erased.  We want to specifically avoid them in the in memory
--   representation and hopefully as we run.

data Table k v = Table
    { _buckets     :: STMMutableArray# RealWorld Any
    , _probeCap    :: Int
    , _threshold   :: Int
    }

readSize :: Bucket k v -> STM Int
readSize b = fromIntegral <$> unsafeReadTStructWord (_unBucket b) 0
{-# INLINE readSize #-}

writeSize :: Bucket k v -> Int -> STM ()
writeSize b s = unsafeWriteTStructWord (_unBucket b) 0 (fromIntegral s)
{-# INLINE writeSize #-}

writeData :: Bucket k v -> Int -> k -> v -> STM ()
writeData b i k v = unsafeWriteTStruct (_unBucket b) (fromIntegral i) (k,v)
{-# INLINE writeData #-}

readData :: Bucket k v -> Int -> STM (k, v)
readData b i = unsafeReadTStruct (_unBucket b) (fromIntegral i)
{-# INLINE readData #-}

copyData :: Bucket k v -> Int -> Int -> STM ()
copyData b source dest = readData b source >>= unsafeWriteTStruct (_unBucket b) (fromIntegral dest)

clear :: Bucket k v -> STM ()
clear b = writeSize b 0

mkBucket :: STM (Bucket k v)
mkBucket = Bucket <$> newTStruct (7 {- CAPACITY -}) 1 (errorWithStackTrace "bucket")

insertBucket :: Bucket k v -> k -> v -> STM ()
insertBucket b k v = do
    s <- readSize b
    writeData b s k v
    writeSize b (s+1)
{-# INLINE insertBucket #-}

removeBucket :: Bucket k v -> Int -> STM ()
removeBucket b i = do
    s <- pred <$> readSize b
    writeSize b s
    copyData b s i
{-# INLINE removeBucket #-}


bucket0 :: Table k v -> Int -> STM (Bucket k v)
bucket0 t (I# i#) = do
    bs <- STM $ \s# -> readTArray# (_buckets t) 0## s#
    let bs# = unsafeCoerce# bs :: Array# Any
    -- Must use a safe read as the hash could be inconsistent with table.
    if isTrue# (i# >=# sizeofArray# bs#)
       then error "bucket0 out of bounds."
       else return (Bucket (TStruct (
                    case indexArray# bs# i# of
                        (# a #) -> unsafeCoerce# a)))
    -- The hope is that this constructor will go away with inlining.
    -- the important thing is to avoid it in the data structure.
{-# INLINE bucket0 #-}

bucket1 :: Table k v -> Int -> STM (Bucket k v)
bucket1 t (I# i#) = do
    bs <- STM $ \s# -> readTArray# (_buckets t) 1## s#
    let bs# = unsafeCoerce# bs :: Array# Any
    -- Must use a safe read as the hash could be inconsistent with table.
    if isTrue# (i# >=# sizeofArray# bs#)
       then error "bucket1 out of bounds."
       else return (Bucket (TStruct (
                    case indexArray# bs# i# of
                        (# a #) -> unsafeCoerce# a)))
    -- The hope is that this constructor will go away with inlining.
    -- the important thing is to avoid it in the data structure.
{-# INLINE bucket1 #-}

bucket :: Int -> Table k v -> Int -> STM (Bucket k v)
bucket 0 t i = bucket0 t i
bucket 1 t i = bucket1 t i
{-# INLINE bucket #-}

upto :: Int -> [Int]
upto 0  = []
upto sz = [0..sz-1]
{-# INLINE upto #-}

mkMutableBucketArray :: Int -> STM (MutableBucketArray k v)
mkMutableBucketArray (I# size#) = STM $ \s1# ->
    case newArray# size# undefined s1# of
           (# s2#, ma# #) -> (# s2#, MutableBucketArray ma# #)
{-# INLINE mkMutableBucketArray #-}

fill :: MutableBucketArray k v -> Int -> Bucket k v -> STM ()
fill a (I# i#) b = STM $ \s1# ->
    case writeArray# (_unMutableBucketArray a) i# (unsafeCoerce# (unTStruct (_unBucket b))) s1# of
      s2# -> (# s2#, () #)
{-# INLINE fill #-}

freezeBucketArray :: MutableBucketArray k v -> STM (BucketArray k v)
freezeBucketArray a = STM $ \s1# ->
    case unsafeFreezeArray# (_unMutableBucketArray a) s1# of
      (# s2#, a# #) -> (# s2#, BucketArray a# #)
{-# INLINE freezeBucketArray #-}

readTableSize :: Table k v -> STM Int
readTableSize t = STM $ \s# ->
    case readTArrayWord# (_buckets t) 0## s# of
      (# s2#, w# #) -> (# s2#, I# (word2Int# w#) #)
{-# INLINE readTableSize #-}

mkTable :: Int -> Int -> Int -> STM (Table k v)
mkTable initSize probeCap threshold = do
    assert "Threshold too small." ((7 {- CAPACITY -}) >= threshold)

    b0 <- mkMutableBucketArray initSize
    b1 <- mkMutableBucketArray initSize
    
    forM_ (upto initSize) $ \i -> do
        mkBucket >>= fill b0 i
        mkBucket >>= fill b1 i

    b0' <- freezeBucketArray b0
    b1' <- freezeBucketArray b1

    bs <- newTStruct 2 1 (unsafeCoerce# (_unBucketArray b0'))
    unsafeWriteTStructP bs 1 (unsafeCoerce# (_unBucketArray b1'))
    unsafeWriteTStructWordP bs 0 (fromIntegral initSize)

    return $ Table
        { _probeCap    = probeCap
        , _threshold   = threshold
        , _buckets     = (unTStruct bs)
        }



assertM s v = do
  b <- v
  unless b $ error s

assert s b = unless b $ error s

class Hashable a where
  toInt :: a -> Int

instance Hashable Int where
  toInt = id

instance Hashable Word where
  toInt = fromIntegral

hash0 :: Hashable a => a -> Int
hash0 k = toInt k*65699
{-# INLINE hash0 #-}

hash1 :: Hashable a => a -> Int
hash1 k = toInt k*65701
{-# INLINE hash1 #-}

hash :: Hashable a => Int -> a -> Int
hash 0 k = hash0 k
hash 1 k = hash1 k
{-# INLINE hash #-}

search :: Eq k => Bucket k v -> k -> STM (Maybe Int) 
search set k = do
    sz <- readSize set
    go sz 0
  where
    go sz i
      | i >= sz   = return Nothing
      | otherwise = do
          k' <- fst <$> readData set i
          if k == k'
            then return $ Just i
            else go sz (i+1)

orM :: Monad m => m Bool -> m Bool -> m Bool
orM ma mb = do
    a <- ma
    if a
      then return True
      else mb
{-# INLINE orM #-}

orMaybe :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
orMaybe ma mb = do
    ma >>= \case
      Nothing -> mb
      x       -> return x
{-# INLINE orMaybe #-}

insert :: (Show k, Show v, Eq k, Hashable k) => Table k v -> k -> v -> STM Bool
insert t k v = do
    sz <- readTableSize t

    let h0 = hash0 k `mod` sz
        h1 = hash1 k `mod` sz

    set0 <- bucket0 t h0
    set1 <- bucket1 t h1

    search set0 k `orMaybe` search set1 k >>= \case
      Just _  -> return False
      Nothing -> do
        b <- tryBucket set0 (_threshold t) `orM` tryBucket set1 (_threshold t)
        if b
          then return True
          else do
            r <- tryProbe set0 (_probeCap t) (0,h0) `orMaybe` tryProbe set1 (_probeCap t) (1,h1)
            case r of
              Nothing    -> do
                resize t
                insert t k v
              Just (i,h) -> do
                b <- relocate t i h
                when b $ resize t
                return True

  where
    tryBucket b threshold = do
      sz <- readSize b
      if sz >= threshold
        then return False
        else do
          insertBucket b k v
          return True

    tryProbe b probeCap a = do
      sz <- readSize b
      if sz >= probeCap
        then return Nothing
        else do
          insertBucket b k v
          return (Just a)

resize :: (Show k, Eq k, Hashable k) => Table k v -> STM ()
resize t = verify t >> error "resize not implemented"
{-
resize t = do
    sz <- readTVar (_size t)
    let sz' = sz*2

    b0' <- newTStruct (fromIntegral sz') 0 (errorWithStackTrace "resize b0'")
    b1' <- newTStruct (fromIntegral sz') 0 (errorWithStackTrace "resize b0'")

    forM_ (upto sz') $ \i -> do
        b <- newTStruct (7 {- CAPACITY -}) 1 (errorWithStackTrace "resize b0' sz")
        unsafeWriteTStruct b0' (fromIntegral i) (Bucket b)

        b <- newTStruct (7 {- CAPACITY -}) 1 (errorWithStackTrace "resize b1' sz")
        unsafeWriteTStruct b1' (fromIntegral i) (Bucket b)

    b0 <- unsafeReadTStruct (_buckets t) 0
    b1 <- unsafeReadTStruct (_buckets t) 1

    writeTVar (_size t) sz'
    unsafeWriteTStruct (_buckets t) 0 b0'
    unsafeWriteTStruct (_buckets t) 1 b1'

    forM_ [b0,b1] $ \bs -> do
      forM_ (upto sz) $ \j -> do
        -- use safe read here as sz and bs could be inconsistent
        set <- readTStruct bs j
        setSize <- readSize set
        forM_ (upto setSize) $ \z -> do
          (k,v) <- readData set z
          set0 <- bucket0 t (hash0 k `mod` sz')
          set1 <- bucket1 t (hash1 k `mod` sz')

          sz0 <- readSize set0
          sz1 <- readSize set1

          if | sz0 < _threshold t -> insertBucket set0 k v
             | sz1 < _threshold t -> insertBucket set1 k v
             | sz0 < _probeCap  t -> insertBucket set0 k v
             | sz1 < _probeCap  t -> insertBucket set1 k v
             | otherwise          -> error "Not enough room after resize!"
-}

relocate :: (Eq k, Hashable k) => Table k v -> Int -> Int -> STM Bool
relocate t i hi = go 8 {- ROUND_LIMIT -} i (1-i) hi
  where
    table i bi = bucket i t bi

    go 0     _ _ _  = return False
    go round i j hi = do
      sz <- readTableSize t
      iSet <- table i hi
      isz <- readSize iSet
      if isz == 0
        then return True
        else do
          (k,_) <- readData iSet (isz-1)
          let hj = hash j k `mod` sz

          jSet <- table j hj
          search iSet k >>= \case
            Just idx -> do
              (k',v') <- readData iSet idx
              removeBucket iSet idx
              jsz <- readSize jSet
              if | jsz < _threshold t -> insertBucket jSet k' v' >> return True
                 | jsz < _probeCap  t -> insertBucket jSet k' v' >> go (round-1) (1-i) (1-j) hj
                 | otherwise          -> insertBucket iSet k' v' >> return False
            Nothing -> if isz >= _threshold t
                          then go (round-1) i j hi -- Has anything changed?
                          else return True

remove :: (Eq k, Hashable k) => Table k v -> k -> STM Bool
remove t k = do
    sz <- readTableSize t

    set0 <- bucket0 t (hash0 k `mod` sz)
    set1 <- bucket1 t (hash1 k `mod` sz)

    search set0 k >>= \case
      Just idx -> removeBucket set0 idx >> return True
      Nothing  -> do
        search set1 k >>= \case
          Just idx -> removeBucket set1 idx >> return True
          Nothing  ->  return False

find :: (Eq k, Hashable k) => Table k v -> k -> STM (Maybe v)
find t k = do
    sz <- fromIntegral <$> readTableSize t

    set0 <- bucket0 t (hash0 k `mod` sz)
    search set0 k >>= \case
      Just idx -> Just . snd <$> readData set0 idx
      Nothing  -> do
        set1 <- bucket1 t (hash1 k `mod` sz)
        search set1 k >>= \case
          Just idx -> Just . snd <$> readData set1 idx
          Nothing  -> return Nothing


verify :: (Show k, Eq k, Hashable k) => Table k v -> STM ()
verify t = do
    sz <- readTableSize t
    forM_ (upto sz) $ \i -> do
      forM_ [0,1] $ \j -> do
        set <- bucket j t i
        szSet <- readSize set
        forM_ (upto szSet) $ \idx -> do
          -- unsafeIOToSTM $ print (i,j,idx)
          (k,_) <- readData set idx
          -- unsafeIOToSTM $ print ("key",k)

          s0 <- bucket0 t (hash0 k `mod` sz)
          s1 <- bucket1 t (hash1 k `mod` sz)

          let s 0 = s0; s 1 = s1

          c <- countKey (s j) k

          assert "Set mismatch" (s j == set)
          assert "duplicate key" (c == 1)

          c' <- countKey (s (1-j)) k

          assert "Key in pair set" (c' == 0)

  where
    countKey :: (Eq k, Show k) => Bucket k v -> k -> STM Int
    countKey set k = do
        sz <- readSize set
        sum <$> forM (upto sz) (\i -> do
            -- unsafeIOToSTM $ print (i,"of",sz)
            (k',_) <- readData set i
            -- unsafeIOToSTM $ print ("count key = ",k')
            return $ if k' == k then 1 else 0)
