{-# LANGUAGE CPP           #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
module CuckooHashTVarSimple
    ( Table
    , mkTable

    , insert
    , remove
    , find
    
    , verify
    ) where

import Control.Applicative
import qualified Control.Concurrent.STM.TArray as T
import qualified Data.Array.MArray as M
import Control.Concurrent.STM
import Control.Monad
import Control.Arrow (second)

import Data.Word

import GHC.Conc
import GHC.Word
import GHC.Int
import GHC.Base hiding (assert)
import GHC.ST

import Data.Array

#define CAPACITY    7
#define ROUND_LIMIT 8

{-
data Array a = Array { _unArray :: Array# a }
data MutableArray a = MutableArray { _unMutableArray :: MutableArray# RealWorld a }

newArray :: Int -> a -> STM (MutableArray a)
newArray (I# i#) a = STM $ \s1# ->
    case newArray# i# a s1# of
        (# s2#, a# #) -> (# s2#, MutableArray a# #)
{-# INLINE newArray #-}

writeArray :: MutableArray a -> Int -> a -> STM ()
writeArray a (I# i#) x = STM $ \s1# ->
    case writeArray# (_unMutableArray a) i# x s1# of
        s2# -> (# s2#, () #)
{-# INLINE writeArray #-}

indexArray :: Array a -> Int -> a
indexArray a (I# i#) =
    case indexArray# (_unArray a) i# of
      (# a #) -> a

unsafeFreezeArray :: MutableArray a -> STM (Array a)
unsafeFreezeArray a = STM $ \s1# ->
    case unsafeFreezeArray# (_unMutableArray a) s1# of 
        (# s2#, a# #) -> (# s2#, Array a# #)
{-# INLINE unsafeFreezeArray #-}

sizeofArray :: Array a -> Int
sizeofArray a = I# (sizeofArray# (_unArray a))
{-# INLINE sizeofArray #-}

growArray :: Array a -> a -> STM (MutableArray a)
growArray a x = do
    let !s@(I# s#) = sizeofArray a
    a' <- newArray (s+1) x
    STM $ \s1# -> 
        case copyArray# (_unArray a) 0# (_unMutableArray a') 0# s# s1# of
            s2# -> (# s2#, a' #)
{-# INLINE growArray #-}

shrinkArray :: Array a -> Int -> STM (MutableArray a)
shrinkArray a i = do
    let !s@(I# s#) = sizeofArray a - 1
    a' <- newArray s undefined
    STM $ \s1# -> 
        case copyArray# (_unArray a) 0# (_unMutableArray a') 0# s# s1# of
            s2# -> (# s2#, () #)
    when (i /= s) $ writeArray a' i (indexArray a s)
    return a'
{-# INLINE shrinkArray #-}
-}

newtype Bucket k v = Bucket { _unBucket :: Array Int (k, v) }
type BucketArray k v = T.TArray Int (Bucket k v)

data Table k v = Table
    { _buckets0  :: {-# UNPACK #-} !(TVar (BucketArray k v))
    , _buckets1  :: {-# UNPACK #-} !(TVar (BucketArray k v))
    , _probeCap  :: {-# UNPACK #-} !Int
    , _threshold :: {-# UNPACK #-} !Int
    }

emptyBucket :: Bucket k v
emptyBucket = Bucket (array (0,-1) [])

bucketSize :: Bucket k v -> Int
bucketSize = succ . uncurry subtract . bounds . _unBucket
{-# INLINE bucketSize #-}

readData :: Bucket k v -> Int -> (k, v)
readData b i = (_unBucket b) ! i
{-# INLINE readData #-}

insertBucket :: Bucket k v -> k -> v -> Bucket k v
insertBucket (Bucket b) k v = Bucket $ listArray (second succ (bounds b)) ((k,v):elems b)
{-# INLINE insertBucket #-}

removeBucket :: Bucket k v -> Int -> Bucket k v
removeBucket (Bucket b) i = Bucket $ listArray (second pred (bounds b)) (hole i $ elems b)
  where
    hole 0 (v:vs) = vs -- v dropped.
    hole i (v:vs) = v : hole (pred i) vs
{-# INLINE removeBucket #-}

bucket0 :: Table k v -> Int -> STM (Bucket k v)
bucket0 t i = do
    bs <- readTVar (_buckets0 t)
    -- Must use a safe read as the hash could be inconsistent with table.
    M.readArray bs i
{-# INLINE bucket0 #-}

bucket1 :: Table k v -> Int -> STM (Bucket k v)
bucket1 t i = do
    bs <- readTVar (_buckets1 t)
    -- Must use a safe read as the hash could be inconsistent with table.
    M.readArray bs i
{-# INLINE bucket1 #-}

bucket :: Int -> Table k v -> Int -> STM (Bucket k v)
bucket 0 t i = bucket0 t i
bucket 1 t i = bucket1 t i
{-# INLINE bucket #-}

update :: Int -> Table k v -> Int -> Bucket k v -> STM ()
update 0 t bi set = do
    b <- readTVar (_buckets0 t)
    M.writeArray b bi set
update 1 t bi set = do
    b <- readTVar (_buckets1 t)
    M.writeArray b bi set
{-# INLINE update #-}

upto :: Int -> [Int]
upto 0  = []
upto sz = [0..sz-1]
{-# INLINE upto #-}

readTableSize :: Table k v -> STM Int
readTableSize t = do
    bs <- readTVar (_buckets0 t)
    (_, i) <- M.getBounds bs
    return $ i+1
{-# INLINE readTableSize #-}

mkTable :: Int -> Int -> Int -> STM (Table k v)
mkTable initSize probeCap threshold = do
    assert "Threshold too small." (CAPACITY >= threshold)

    let bounds = (0, initSize-1)
    b0 <- M.newArray bounds emptyBucket
    b1 <- M.newArray bounds emptyBucket

    t0 <- newTVar b0
    t1 <- newTVar b1

    return $ Table
        { _probeCap  = probeCap
        , _threshold = threshold
        , _buckets0  = t0
        , _buckets1  = t1
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
search set k = go (bucketSize set) 0
  where
    go sz i
      | i >= sz   = return Nothing
      | otherwise = do
          let k' = fst (readData set i)
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

    let up0 = update 0 t h0
        up1 = update 1 t h1

    search set0 k `orMaybe` search set1 k >>= \case
      Just _  -> return False
      Nothing -> do
        b <- tryBucket set0 (_threshold t) up0 `orM` tryBucket set1 (_threshold t) up1
        if b
          then return True
          else do
            r <- tryProbe set0 (_probeCap t) (0,h0) up0 `orMaybe` tryProbe set1 (_probeCap t) (1,h1) up1
            case r of
              Nothing    -> do
                resize t
                insert t k v
              Just (i,h) -> do
                b <- relocate t i h
                when b $ resize t
                return True

  where
    tryBucket b threshold assign = do
      if bucketSize b >= threshold
        then return False
        else do
          assign (insertBucket b k v)
          return True

    tryProbe b probeCap a assign = do
      if bucketSize b >= probeCap
        then return Nothing
        else do
          assign (insertBucket b k v)
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
relocate t i hi = go ROUND_LIMIT i (1-i) hi
  where
    table i bi = bucket i t bi

    go 0     _ _ _  = return False
    go round i j hi = do
      sz <- readTableSize t
      iSet <- table i hi
      let isz = bucketSize iSet
      if isz == 0
        then return True
        else do
          let (k,_) = readData iSet (isz-1)
          let hj = hash j k `mod` sz

          jSet <- table j hj
          search iSet k >>= \case
            Just idx -> do
              let (k',v') = readData iSet idx
              update i t hi (removeBucket iSet idx)
              let jsz = bucketSize jSet
              if | jsz < _threshold t -> update j t hj (insertBucket jSet k' v') >> return True
                 | jsz < _probeCap  t -> update j t hj (insertBucket jSet k' v') 
                                                            >> go (round-1) (1-i) (1-j) hj
                 | otherwise          -> update j t hj (insertBucket iSet k' v') >> return False
            Nothing -> if isz >= _threshold t
                          then go (round-1) i j hi -- Has anything changed?
                          else return True


remove :: (Eq k, Hashable k) => Table k v -> k -> STM Bool
remove t k = do
    sz <- readTableSize t

    let h0 = hash0 k `mod` sz
        h1 = hash1 k `mod` sz

    set0 <- bucket0 t h0
    set1 <- bucket1 t h1

    search set0 k >>= \case
      Just idx -> update 0 t h0 (removeBucket set0 idx) >> return True
      Nothing  -> do
        search set1 k >>= \case
          Just idx -> update 1 t h1 (removeBucket set1 idx) >> return True
          Nothing  -> return False

find :: (Eq k, Hashable k) => Table k v -> k -> STM (Maybe v)
find t k = do
    sz <- fromIntegral <$> readTableSize t

    set0 <- bucket0 t (hash0 k `mod` sz)
    search set0 k >>= \case
      Just idx -> return . Just . snd $ readData set0 idx
      Nothing  -> do
        set1 <- bucket1 t (hash1 k `mod` sz)
        search set1 k >>= \case
          Just idx -> return . Just . snd $ readData set1 idx
          Nothing  -> return Nothing


verify :: (Show k, Eq k, Hashable k) => Table k v -> STM ()
verify t = do
    sz <- readTableSize t
    forM_ (upto sz) $ \i -> do
      forM_ [0,1] $ \j -> do
        set <- bucket j t i
        let szSet = bucketSize set
        forM_ (upto szSet) $ \idx -> do
          -- unsafeIOToSTM $ print (i,j,idx)
          let (k,_) = readData set idx
          -- unsafeIOToSTM $ print ("key",k)

          s0 <- bucket0 t (hash0 k `mod` sz)
          s1 <- bucket1 t (hash1 k `mod` sz)

          let s 0 = s0; s 1 = s1

          c <- countKey (s j) k

          -- assert "Set mismatch" (s j == set)
          assert "duplicate key" (c == 1)

          c' <- countKey (s (1-j)) k

          assert "Key in pair set" (c' == 0)

  where
    countKey :: (Eq k, Show k) => Bucket k v -> k -> STM Int
    countKey set k = do
        let sz = bucketSize set
        sum <$> forM (upto sz) (\i -> do
            -- unsafeIOToSTM $ print (i,"of",sz)
            let (k',_) = readData set i
            -- unsafeIOToSTM $ print ("count key = ",k')
            return $ if k' == k then 1 else 0)
