module Reservation
    ( ReservationType(..)
    , ReservationInfo(..)
    , Reservation(..)

    , mkReservation
    , addToTotal
    , make
    , cancel
    , updatePrice
    ) where

import Control.Monad
import Control.Applicative

import Control.Concurrent
import Control.Concurrent.STM

import Data.Function

data ReservationType = Car | Flight | Room
    deriving (Enum, Bounded, Eq, Ord, Show, Read)

data ReservationInfo = ReservationInfo 
    { _infoType  :: ReservationType
    , _infoID    :: Int
    , _infoPrice :: Int
    }
    deriving (Show, Read)

data Reservation = Reservation
    { _id    :: Int
    , _used  :: TVar Int
    , _free  :: TVar Int
    , _total :: TVar Int
    , _price :: TVar Int
    }

instance Eq Reservation where
  (==) = (==) `on` _id

instance Ord Reservation where
  (<=) = (<=) `on` _id

instance Eq ReservationInfo where
  (ReservationInfo t id _) == (ReservationInfo t' id' _) = t == t' && id == id'

instance Ord ReservationInfo where
  compare (ReservationInfo t id _) (ReservationInfo t' id' _)
    = case compare t t' of
        EQ -> compare id id'
        x  -> x

checkVar :: TVar a -> (a -> Bool) -> STM a
checkVar t f = do
    v <- readTVar t
    unless (f v) retry
    return v

checkReservation :: Reservation -> STM ()
checkReservation r = do
    u <- checkVar (_used  r) (>= 0)
    f <- checkVar (_free  r) (>= 0)
    t <- checkVar (_total r) (>= 0)

    when (u + f /= t) retry

    checkVar (_price r) (>= 0)
    return ()

mkReservation :: Int -> Int -> Int -> STM Reservation
mkReservation id t p = do
    r <- Reservation id <$> newTVar 0 <*> newTVar t <*> newTVar t <*> newTVar p
    checkReservation r
    return r

addToTotal :: Reservation -> Int -> STM Bool
addToTotal r n = do
    f <- readTVar (_free r)
    if f + n < 0
      then return False
      else do
        writeTVar  (_free  r) (f + n)
        modifyTVar (_total r) (+n)

        checkReservation r

        return True

make :: Reservation -> STM Bool
make r = do
    f <- readTVar (_free r)
    if f < 1
      then return False
      else do
        modifyTVar (_used r) (+1)
        writeTVar  (_free r) (f-1)

        checkReservation r

        return True

cancel :: Reservation -> STM Bool
cancel r = do
    u <- readTVar (_used r)
    if u < 1
      then return False
      else do
        writeTVar  (_used r) (u-1)
        modifyTVar (_free r) (+1)

        checkReservation r

        return True

updatePrice :: Reservation -> Int -> STM Bool
updatePrice r p = do
    if p < 0
      then return False
      else do
        writeTVar (_price r) p
        checkReservation r
        return True


