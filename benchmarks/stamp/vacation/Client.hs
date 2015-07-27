{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns    #-}
module Client 
    ( Client
    , mkClient

    , runClient
    , countClient
    ) where

import Manager
import Reservation
import RandomMWC
import Customer

import Control.Monad
import Control.Applicative

import Control.Concurrent
import Control.Concurrent.STM

import Data.Semigroup
import Data.IORef

data Client = Client 
    { _id          :: Int
    , _manager     :: Manager
    , _random      :: Random
    , _operations  :: Int
    , _queries     :: Int
    , _range       :: Int
    , _percentUser :: Int
    , _count       :: IORef Int
    }

mkClient :: Int -> Manager -> Int -> Int -> Int -> Int -> IO Client
mkClient id m operations queries range percent = do
    r <- initRandom . fromIntegral $ id
    c <- newIORef 0
    return $ Client id m r operations queries range percent c

countClient :: Client -> IO Int
countClient = readIORef . _count

foldSTM :: Monoid b => (a -> STM b) -> [a] -> STM b
foldSTM _ []     = return mempty
foldSTM f ((!a):(!as)) = do
    !r <- f a
    (r `mappend`) <$> foldSTM f as

atomically' count act = do
    atomically act
    modifyIORef' count succ

runClient :: Client -> IO ()
runClient Client{..} = do
    if _operations < 0
      then go
      else forM_ [0.._operations-1] $ \_ -> do
              !r <- (`mod` 100) . fromIntegral <$> getRandom _random
              selectAction r
  where
    go = do
        !r <- (`mod` 100) . fromIntegral <$> getRandom _random
        selectAction r
        go

    selectAction r
      | r < _percentUser = actionMakeReservation
      | odd r            = actionDeleteCustomer
      | otherwise        = actionUpdateTables

    actionMakeReservation = do
        !n <- (+1) . (`mod` _queries) . fromIntegral <$> getRandom _random
        !customerID <- (+1) . (`mod` _range) . fromIntegral <$> getRandom _random
        !as <- forM [0..n - 1] $ \_ -> do
            !t  <- toEnum . fromIntegral . (`mod` 3) <$> getRandom _random
            !id <- (+1) . (`mod` _range) . fromIntegral <$> getRandom _random
            return (t,id)
        atomically' _count $ do
            r <- foldSTM act as
            case r of
              (Option Nothing,Option Nothing,Option Nothing) -> return ()
              (car,room,flight) -> do
                addCustomer _manager customerID
                reserve Car    customerID car
                reserve Room   customerID room
                reserve Flight customerID flight
      where
        act  (t,id) = do
            let (q,qp) = pick t
            r <- q _manager id
            case r of
              Just v | v >= 0 -> (\p -> wrap t . Option . Just . Max $ (p,id)) 
                                   <$> qp _manager id
              _               -> return (empty,empty,empty)
        
        reserve _ _ (Option Nothing) = return ()
        reserve t cid (Option (Just (Max (p,id)))) = pickReserve t _manager cid id >> return ()
        
        pickReserve Car    = reserveCar
        pickReserve Room   = reserveRoom
        pickReserve Flight = reserveFlight
              
        pick Car    = (queryCar, queryCarPrice)
        pick Room   = (queryRoom, queryRoomPrice)
        pick Flight = (queryFlight, queryFlightPrice)
        
        wrap Car    = (,empty,empty)
        wrap Room   = (empty,,empty)
        wrap Flight = (empty,empty,)

    actionDeleteCustomer = do
        !id <- (+1) . (`mod` _range) . fromIntegral <$> getRandom _random
        atomically' _count $ do
            b <- queryCustomerBill _manager id
            case b of
                Just v | v >= 0 -> deleteCustomer _manager id >> return ()
                _               -> return ()
    
    actionUpdateTables = do
        !updates <- (+1) . (`mod` _queries) . fromIntegral <$> getRandom _random
        us <- forM [0..updates - 1] $ \_ -> do
            !t  <- toEnum . fromIntegral . (`mod` 3) <$> getRandom _random
            !id <- (+1) . (`mod` _range) . fromIntegral <$> getRandom _random
            !op <- odd <$> getRandom _random
            !p <- if op
                   then (+50) . (*10) . (`mod` 5) . fromIntegral <$> getRandom _random
                   else return 0
            return (t,id,op,p)
        atomically' _count $ forM_ us ((return () >>) . act)
      where
        act (Car,   id,True,p) = addCar    _manager id 100 p
        act (Room,  id,True,p) = addRoom   _manager id 100 p
        act (Flight,id,True,p) = addFlight _manager id 100 p
            
        act (Car,   id,_,_) = deleteCar    _manager id 100
        act (Room,  id,_,_) = deleteRoom   _manager id 100
        act (Flight,id,_,_) = deleteFlight _manager id
