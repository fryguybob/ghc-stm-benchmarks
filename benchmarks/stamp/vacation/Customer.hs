module Customer
    ( Customer(..)

    , addReservationInfo
    , removeReservationInfo
    , getBill
    ) where

import TList
import Reservation

import Control.Monad
import Control.Applicative

import Control.Concurrent
import Control.Concurrent.STM

import Data.Monoid

data Customer = Customer 
    { _customerID       :: Int
    , _reservationInfos :: TList ReservationInfo
    }

instance Show Customer where
    show (Customer i _) = "C" ++ show i

instance Eq Customer where
  (Customer a _) == (Customer b _) = a == b

instance Ord Customer where
  (Customer a _) <= (Customer b _) = a <= b

addReservationInfo :: Customer -> ReservationType -> Int -> Int -> STM Bool
addReservationInfo c t id p = insert (_reservationInfos c) (ReservationInfo t id p)

removeReservationInfo :: Customer -> ReservationType -> Int -> STM Bool
removeReservationInfo (Customer _ is) t id = do
    let a = ReservationInfo t id 0
    i <- find is a
    case i of
      Nothing -> return False
      _       -> do
        remove is a >>= (`unless` retry)
        return True

getBill :: Customer -> STM Int
getBill (Customer id is) = getSum <$> foldMapTList (Sum . _infoPrice) is
