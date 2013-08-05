module Manager 
    (
      Manager
    , mkManager

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
    
    , addCustomer, deleteCustomer
    
    , checkUniqueCustomers, checkUniqueTables
    ) where

import RBTree
import qualified TList as L
import Reservation hiding (cancel)
import qualified Reservation as R
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

assertM :: Monad m => String -> Bool -> m ()
assertM s b = when (not b) $ error s

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
                     assertM "addReservation Insert Failed" b
                     return True
      Just r -> do
        b <- addToTotal r num
        if not b
          then return False
          else do
            total <- readTVar (_total r)
            if total /= 0
              then updatePrice r price >> return True
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
        i <- insert (customerTable m) id (Customer id l)
        verify (customerTable m)
        return i

deleteCustomer :: Manager -> Int -> STM Bool
deleteCustomer m id = do
    r <- get (customerTable m) id
    case r of
      Nothing -> return False
      Just (Customer _ l) -> do
        L.forEach l $ \(ReservationInfo i id p) -> do
          mr <- get (pickTable i m) id
          case mr of
            Nothing -> retry
            Just r  -> R.cancel r >>= (`unless` retry)
        delete (customerTable m) id >>= (`unless` retry)
        verify (customerTable m)
        return True

pickTable Car    = carTable
pickTable Flight = flightTable
pickTable Room   = roomTable

queryField :: (Reservation -> TVar a) -> TMap Reservation -> Int -> STM (Maybe a)
queryField f t id = do
    r <- get t id
    case r of
      Nothing -> return Nothing
      Just r  -> Just <$> readTVar (f r)

queryNumberFree :: TMap Reservation -> Int -> STM (Maybe Int)
queryNumberFree = queryField _free

queryPrice :: TMap Reservation -> Int -> STM (Maybe Int)
queryPrice = queryField _price

queryCar, queryRoom,  queryFlight :: Manager -> Int -> STM (Maybe Int)
queryCar    = queryNumberFree . carTable
queryRoom   = queryNumberFree . roomTable
queryFlight = queryNumberFree . flightTable

queryCarPrice, queryRoomPrice, queryFlightPrice :: Manager -> Int -> STM (Maybe Int)
queryCarPrice    = queryPrice . carTable
queryRoomPrice   = queryPrice . roomTable
queryFlightPrice = queryPrice . flightTable

queryCustomerBill :: Manager -> Int -> STM (Maybe Int)
queryCustomerBill m id = do
    r <- get (customerTable m) id
    case r of
      Nothing -> return Nothing
      Just c  -> Just <$> getBill c

reserve :: TMap Reservation -> TMap Customer -> Int -> Int -> ReservationType -> STM Bool
reserve tr tc cid id rt = do
    rc <- get tc cid
    case rc of
      Nothing -> return False
      Just c  -> do
        rr <- get tr id
        case rr of
          Nothing -> return False
          Just r  -> do
            b' <- make r
            if not b'
              then return False
              else do
                p <- readTVar (_price r)
                b <- addReservationInfo c rt id p
                if b
                  then return True
                  else do
                    R.cancel r >>= (`unless` retry)
                    return False

reserveCar, reserveRoom, reserveFlight :: Manager -> Int -> Int -> STM Bool
reserveCar    m cid id = reserve (carTable m)    (customerTable m) cid id Car
reserveRoom   m cid id = reserve (roomTable m)   (customerTable m) cid id Room
reserveFlight m cid id = reserve (flightTable m) (customerTable m) cid id Flight

cancel :: TMap Reservation -> TMap Customer -> Int -> Int -> ReservationType -> STM Bool
cancel tr tc cid id rt = do
    rc <- get tc cid
    case rc of
      Nothing -> return False
      Just c  -> do
        rr <- get tr id
        case rr of
          Nothing -> return False
          Just r  -> do
            rc <- R.cancel r
            if not rc
              then return False
              else do
                b <- removeReservationInfo c rt id
                if b
                  then return True
                  else do
                    make r >>= (`unless` retry)
                    return False

cancelCar, cancelRoom, cancelFlight :: Manager -> Int -> Int -> STM Bool
cancelCar    m cid id = cancel (carTable m)    (customerTable m) cid id Car
cancelRoom   m cid id = cancel (roomTable m)   (customerTable m) cid id Room
cancelFlight m cid id = cancel (flightTable m) (customerTable m) cid id Flight

checkUniqueCustomers :: Manager -> Int -> IO ()
checkUniqueCustomers m n = 
    forM_ [1..n] $ \i -> do 
      atomically $ verify (customerTable m)
      atomically $ do
        r <- get (customerTable m) i
        case r of
          Nothing -> return ()
          Just c  -> do
            b <- delete (customerTable m) i
            when b $ do
              r <- get (customerTable m) i
              case r of
                Nothing -> return ()
                _       -> error "Duplicate customer."

checkUniqueTables :: Manager -> Int -> IO ()
checkUniqueTables m n = do
    atomically $ checkTable (carTable    m) n
    atomically $ checkTable (roomTable   m) n
    atomically $ checkTable (flightTable m) n

checkTable :: TMap Reservation -> Int -> STM ()
checkTable t n =
    forM_ [1..n] $ \i -> do
      r <- get t i
      case r of
        Nothing -> return ()
        Just c  -> do
          b <- addReservation t i 0 0
          assertM "checkTable addReservation failed" b
          r <- delete t i
          case r of
            False -> return ()
            _     -> do
                b <- delete t i
                assertM "checkTable delete failed" (not b)

