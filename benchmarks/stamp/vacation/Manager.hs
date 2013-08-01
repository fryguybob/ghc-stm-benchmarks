module Manager 
    (
      Manager
    , mkManager
{-
    , queryNumberFree
    , queryPrice

    , reserve
    , addReservation

    , addCar   , addRoom   , addFlight
    , deleteCar, deleteRoom, deleteFlight

    , queryCar , queryRoom , queryFlight
    , queryCarPrice, queryRoomPrice, queryFlightPrice

    , queryCustomerBill

    , reserveCar, reserveRoom, reserveFlight
    , cancelCar, cancelRoom, cancelFlight
-}
    ) where

import RBTree
import qualified TList as L
import Reservation
import Customer

import Control.Applicative
import Control.Monad
import Control.Exception

import Control.Concurrent.STM
import Control.Concurrent

type TMap = RBTree Int

newTMap :: STM (TMap a)
newTMap = mkRBTree

data Manager = Manager
    { carTable      :: TMap Reservation
    , roomTable     :: TMap Reservation
    , flightTable   :: TMap Reservation
    , customerTable :: TMap Customer
    }

assertM :: Monad m => Bool -> m ()
assertM b = assert b (return ())

mkManager :: IO Manager
mkManager = atomically $ Manager <$> newTMap <*> newTMap <*> newTMap <*> newTMap

addReservation :: TMap Reservation -> Int -> Int -> Int -> STM Bool
addReservation t id num price = do
    r <- get t id
    case r of
      Nothing -> if num < 1 || price < 0
                   then return False
                   else do
                     r <- mkReservation id num price
                     b <- insert t id r
                     assertM b
                     return True
      Just r -> do
        b <- addToTotal r num
        if b
          then return False
          else do
            total <- readTVar (_total r)
            if total /= 0
              then updatePrice r price
              else do
                b <- delete t id
                when (not b) retry
                return True

addCar, addRoom, addFlight :: Manager -> Int -> Int -> Int -> STM Bool
addCar    m = addReservation (carTable    m)
addRoom   m = addReservation (roomTable   m)
addFlight m = addReservation (flightTable m)

deleteCar, deleteRoom :: Manager -> Int -> Int -> STM Bool
deleteCar    m id num = addReservation (carTable    m) id (-num) (-1)
deleteRoom   m id num = addReservation (roomTable   m) id (-num) (-1)

deleteFlight :: Manager -> Int -> STM Bool
deleteFlight m id = do
    r <- get (flightTable m) id
    case r of
      Nothing -> return False
      Just r  -> do
        u <- readTVar (_used r)
        if u > 0
          then return False
          else do
            t <- readTVar (_total r)
            addReservation (flightTable m) id (-t) (-1)

addCustomer :: Manager -> Int -> STM Bool
addCustomer m id = do
    b <- contains (customerTable m) id
    if b
      then return False
      else do
        l <- L.mkTList
        insert (customerTable m) id (Customer id l)

deleteCustomer :: Manager -> Int -> STM Bool
deleteCustomer m id = do
    r <- get (customerTable m) id
    case r of
      Nothing -> return False
      Just (Customer _ l) -> do
        L.forEach l $ \(ReservationInfo i id p) -> do
          mr <- get (pickTable i) id
          case mr of
            Nothing -> retry
            Just r  -> cancel r >>= (`unless` retry)
        delete (customerTable m) id >>= (`unless` retry)
        return True
  where
    pickTable Car    = carTable m
    pickTable Flight = flightTable m
    pickTable Room   = roomTable m
