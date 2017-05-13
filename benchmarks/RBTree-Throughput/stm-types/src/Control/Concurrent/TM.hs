{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Concurrent.TM
    ( TM(..)
    , atomically
    , atomicallyReadOnly
    , retry
    , retryReadOnly
    , throwTM
    , catchTM
    , TVar(..)
    , newTVar
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    ) where

import GHC.Base
import GHC.Conc
import GHC.Prim

import Control.Monad.Indexed

data ReadOnly
data ReadWrite

type family MaxRW a b where
    MaxRW ReadOnly  ReadOnly  = ReadOnly
    MaxRW ReadWrite b         = ReadWrite
    MaxRW a         ReadWrite = ReadWrite

newtype TM i j a = TM { unTM ::(State# RealWorld -> (# State# RealWorld, a #)) }

instance IxFunctor TM where
  imap f (TM act) = TM $ \s1# ->
    case act s1# of
      (# s2#, a #) -> (# s2#, f a #)


instance IxPointed TM where
  ireturn a = TM $ \s# -> (# s#, a #)


instance IxApplicative TM where
  iap (TM actf) acta = TM $ \s1# ->
    case act s1# of
      (# s2#, f #) ->
        case acta s2# of
          (# s3#, a #) -> (# s3#, f a #)


instance IxMonad TM where
  ibind f (TM act) = TM $ \s1# ->
    case act s1# of
      (# s2#, a #) -> unTM (f a) s2#


newTVar :: TM i i (TVar a)
newTVarIO :: IO (TVar a)

readTVar :: TVar a -> TM i i a
writeTVar :: TVar a -> a -> TM i ReadWrite a

-- Atomic blocks where the read only varient potentially has a
-- more efficient implementation (no write-set tracking needed)
atomically :: TM i j a -> IO a
atomicallyReadOnly :: TM i ReadOnly a -> IO a

-- Atomic blocks which are more agressively attempted on HTM.
atomicallyHTM :: TM i j a -> IO a
atomicallyReadOnlyHTM :: TM i ReadOnly a -> IO a

-- Atomic blocks which do not attempt hardware transactions.
atomicallySTM :: TM i j a -> IO a
atomicallyReadOnlySTM :: TM i ReadOnly a -> IO a

orElse :: TM i j a -> TM i k a -> TM i (MaxRW j k) a

orElseReadOnly :: TM i ReadOnly a -> TM i ReadOnly a -> TM i ReadOnly a

-- Potentially you could have the type `TM i ReadOnly a` here as the transaction
-- will be rolled back, but I can't think of a situation where this would be useful
-- for code that people write.
throwTM :: Exception e => e -> TM i i a

catchTM :: Exception e => TM i j a -> (e -> TM i k a) -> TM i (MaxRW j k) a

retry :: TM i i a

retryReadOnly :: TM ReadOnly ReadOnly a
