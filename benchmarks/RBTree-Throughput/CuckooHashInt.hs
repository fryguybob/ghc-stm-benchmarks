{-# LANGUAGE CPP          #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE BangPatterns #-}
module CuckooHashInt
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
import GHC.Stack

import TStruct

#define CAPACITY    7
#define ROUND_LIMIT 8

assertM s v = do
  b <- v
  unless b $ error s

assert s b = unless b $ error s

type Key = Word


-- Alternatively we can specalize this to some fixed representation of 
-- key and value and store it all in words.
newtype Bucket v = Bucket { _data :: TStruct v }
    deriving (Eq)

readSize :: Bucket v -> STM Word
readSize b = unsafeReadTStructWord (_data b) 0
{-# INLINE readSize #-}

writeSize :: Bucket v -> Word -> STM ()
writeSize b s = unsafeWriteTStructWord (_data b) 0 s
{-# INLINE writeSize #-}

writeData :: Bucket v -> Word -> Key -> v -> STM ()
writeData b i k v = do
    unsafeWriteTStruct (_data b) i v
    unsafeWriteTStructWord (_data b) (i+1) k
{-# INLINE writeData #-}

readData :: Bucket v -> Word -> STM (k, v)
readData b i = do
    k <- unsafeReadTStructWord (_data b) (i+1)
    v <- unsafeReadTStruct     (_data b) i
{-# INLINE readData #-}

copyData :: Bucket v -> Word -> Word -> STM ()
copyData b source dest = do
    unsafeReadTStruct (_data b) source >>= unsafeWriteTStruct (_data b) dest
    unsafeReadTStructWord (_data b) (source+1) >>= unsafeWriteTStructWord (_data b) (dest+1)

clear :: Bucket v -> STM ()
clear b = writeSize b 0

mkBucket :: STM (Bucket v)
mkBucket = Bucket <$> newTStruct (1+CAPACITY) CAPACITY (errorWithStackTrace "bucket")

insertBucket :: Bucket v -> Key -> v -> STM ()
insertBucket b k v = do
    s <- readSize b
    writeData b s k v
    writeSize b (s+1)
{-# INLINE insert #-}

removeBucket :: Bucket v -> Word -> STM ()
removeBucket b i = do
    s <- pred <$> readSize b
    writeSize b s
    copyData b s i
{-# INLINE remove #-}

data Table v = Table 
    { _buckets     :: {-# UNPACK #-} !TStruct (TStruct (Bucket v))
    , _size        :: {-# UNPACK #-} !TVar Word
    , _probeCap    :: {-# UNPACK #-} !Word
    , _threshold   :: {-# UNPACK #-} !Word
    }

class Hashable a where
  toWord :: a -> Word

instance Hashable Word where
  toWord = id

instance Hashable Int where
  toWord = fromIntegral

hash0 :: Hashable a => a -> Word
hash0 k = toWord k*65699
{-# INLINE hash0 #-}

hash1 :: Hashable a => a -> Word
hash1 k = toWord k*65701
{-# INLINE hash1 #-}

hash :: Hashable a => Word -> a -> Word
hash 0 k = hash0 k
hash 1 k = hash1 k
{-# INLINE hash #-}

bucket0 :: Table v -> Word -> STM (Bucket v)
bucket0 t i = do
    bs <- unsafeReadTStruct (_buckets t) 0
    -- Must use a safe read as the hash could be inconsistent with table.
    readTStruct bs i 
{-# INLINE bucket0 #-}

bucket1 :: Table v -> Word -> STM (Bucket v)
bucket1 t i = do
    bs <- unsafeReadTStruct (_buckets t) 1
    -- Must use a safe read as the hash could be inconsistent with table.
    readTStruct bs i
{-# INLINE bucket1 #-}

bucket :: Word -> Table v -> Word -> STM (Bucket v)
bucket b t i = do
    bs <- unsafeReadTStruct (_buckets t) b
    -- Must use a safe read as the hash could be inconsistent with table.
    readTStruct bs i
{-# INLINE bucket #-}

search :: Bucket v -> Key -> STM (Maybe Word) 
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

upto :: Word -> [Word]
upto 0  = []
upto sz = [0..sz-1]


mkTable :: Word -> Word -> Word -> STM (Table v)
mkTable initSize probeCap threshold = do
    assert "Threshold too small." (CAPACITY >= threshold)

    b0 <- newTStruct (fromIntegral initSize) 0 (errorWithStackTrace "mkTable b0" :: Bucket k v)
    b1 <- newTStruct (fromIntegral initSize) 0 (errorWithStackTrace "mkTable b1")


    forM_ (upto initSize) $ \i -> do
        b <- newTStruct CAPACITY 1 (errorWithStackTrace "mkTable b0 sz")
        unsafeWriteTStruct b0 i (Bucket b)

        b <- newTStruct CAPACITY 1 (errorWithStackTrace "mkTable b1 sz")
        unsafeWriteTStruct b1 i (Bucket b)

    bs <- newTStruct 2 0 b0
    unsafeWriteTStruct bs 1 b1

    sz <- newTVar initSize

    return $ Table
        { _size        = sz
        , _probeCap    = probeCap
        , _threshold   = threshold
        , _buckets     = bs
        }

orM :: Monad m => m Bool -> m Bool -> m Bool
orM ma mb = do
    a <- ma
    if a
      then return True
      else mb

orMaybe :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
orMaybe ma mb = do
    ma >>= \case
      Nothing -> mb
      x       -> return x

insert :: Table v -> Key -> v -> STM Bool
insert t k v = do
    sz <- readTVar (_size t)

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

resize :: Table v -> STM ()
resize t = do
    sz <- readTVar (_size t)
    let sz' = sz*2

    b0' <- newTStruct (fromIntegral sz') 0 (errorWithStackTrace "resize b0'")
    b1' <- newTStruct (fromIntegral sz') 0 (errorWithStackTrace "resize b0'")

    forM_ (upto sz') $ \i -> do
        b <- newTStruct CAPACITY 1 (errorWithStackTrace "resize b0' sz")
        unsafeWriteTStruct b0' i (Bucket b)

        b <- newTStruct CAPACITY 1 (errorWithStackTrace "resize b1' sz")
        unsafeWriteTStruct b1' i (Bucket b)

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

relocate :: Table v -> Word -> Word -> STM Bool
relocate t i hi = go ROUND_LIMIT i (1-i) hi
  where
    table i bi = do
        bs <- unsafeReadTStruct (_buckets t) i
        -- Safe read needed!
        readTStruct bs bi

    go 0     _ _ _  = return False
    go round i j hi = do
      sz <- readTVar (_size t)
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

remove :: Table v -> Key -> STM Bool
remove t k = do
    sz <- readTVar (_size t)

    set0 <- bucket0 t (hash0 k `mod` sz)
    set1 <- bucket1 t (hash1 k `mod` sz)

    search set0 k >>= \case
      Just idx -> removeBucket set0 idx >> return True
      Nothing  -> do
        search set1 k >>= \case
          Just idx -> removeBucket set1 idx >> return True
          Nothing  ->  return False

find :: Table v -> Key -> STM (Maybe v)
find t k = do
    sz <- readTVar (_size t)

    set0 <- bucket0 t (hash0 k `mod` sz)
    set1 <- bucket1 t (hash1 k `mod` sz)

    search set0 k >>= \case
      Just idx -> Just . snd <$> readData set0 idx
      Nothing  -> do
        search set1 k >>= \case
          Just idx -> Just . snd <$> readData set1 idx
          Nothing  -> return Nothing
    
verify :: Table v -> STM ()
verify t = do
    sz <- readTVar (_size t)
    forM_ (upto sz) $ \i -> do
      forM_ [0,1] $ \j -> do
        set <- bucket j t i
        szSet <- readSize set
        forM_ (upto szSet) $ \idx -> do
--          unsafeIOToSTM $ print (i,j,idx)
          (k,_) <- readData set idx
--          unsafeIOToSTM $ print ("key",k)

          s0 <- bucket0 t (hash0 k `mod` sz)
          s1 <- bucket1 t (hash1 k `mod` sz)

          let s 0 = s0; s 1 = s1

          c <- countKey (s j) k

          assert "Set mismatch" (s j == set)
          assert "duplicate key" (c == 1)

          c' <- countKey (s (1-j)) k

          assert "Key in pair set" (c' == 0)

  where
    countKey :: (Eq k, Show k) => Bucket k v -> k -> STM Word
    countKey set k = do
        sz <- readSize set
        sum <$> forM (upto sz) (\i -> do
--            unsafeIOToSTM $ print (i,"of",sz)
            (k',_) <- readData set i
--            unsafeIOToSTM $ print ("count key = ",k')
            return $ if k' == k then 1 else 0)
