{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Random
import Reservation
import Customer
import Manager
import Client

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

import System.IO

import GHC.Conc

import Data.Array.IO

import System.Environment (getProgName)
import System.Directory (doesFileExist)

import System.Console.CmdArgs.Implicit

data VacationOpts = VacationOpts
    { clients      :: Int
    , number       :: Int
    , queries      :: Int
    , relations    :: Int
    , transactions :: Int
    , user         :: Int
    } deriving (Show, Data, Typeable)

vacationOpts prog = VacationOpts
    { clients      = 1
                  &= name "c"
                  &= help "Number of clients"
    , number       = 10
                  &= name "n"
                  &= help "number of user queries/transaction"
    , queries      = 90
                  &= name "q"
                  &= help "Percentage of relations queried"
    , relations    = (2^16 :: Int)
                  &= name "r"
                  &= help "Number of possible relations"
    , transactions = (2^26 :: Int)
                  &= name "t"
                  &= help "Number of transactions"
    , user         = 80
                  &= name "u"
                  &= help "Percentage of user transactions"
    }
    &= program prog

dup :: Applicative m => m a -> m (a,a)
dup a = (,) <$> a <*> a

shuffle :: Random -> [Int] -> IO [Int]
shuffle r is = do
    let l = length is
    
    as <- newListArray (0,l-1) is :: IO (IOUArray Int Int)

    -- Matching the biased shuffle from STAMP
    forM_ is $ \_ -> do
        (x,y) <- dup $ (`mod` l) . fromIntegral <$> getRandom r
        vx <- readArray as x
        vy <- readArray as y
        writeArray as x vy
        writeArray as y vx

    getElems as

phase :: String -> IO a -> IO a
phase m a = do
    putStr (m++"... ")
    hFlush stdout
    r <- a
    putStrLn "done."
    return r

initializeManager :: Random -> Int -> IO Manager
initializeManager r relations = phase "Initalizing manager" $ do
    m <- mkManager

    let numTable = length managerAdd
        managerAdd = [ addCar
                     , addFlight
                     , addRoom
                     , \m i _ _ -> addCustomer m i
                     ]

    forM_ managerAdd $ \add -> do
        is <- shuffle r [1..relations]
        
        forM_ is $ \i -> do
            num   <- (*100) . (+1) . (`mod` 5) . fromIntegral <$> getRandom r
            price <- (+50) . (*10) . (`mod` 5) . fromIntegral <$> getRandom r
            atomically $ add m i num price

    return m

initalizeClients :: Manager -> VacationOpts -> IO [Client]
initalizeClients m VacationOpts{..} = do
    cs <- phase "Initializing clients" $ do
        forM [0..clients-1] $ \i -> mkClient i m ts number range user
    mapM_ putStrLn $
        [ "    Transactions        = " ++ show transactions
        , "    Clients             = " ++ show clients
        , "    Transactions/client = " ++ show ts
        , "    Queries/transaction = " ++ show number
        , "    Relations           = " ++ show relations
        , "    Query percent       = " ++ show queries
        , "    Query range         = " ++ show range
        , "    Percent user        = " ++ show user
        ]
    return cs
  where
    ts    = round (fromIntegral transactions / fromIntegral clients)
    range = round (fromIntegral queries / 100.0 * fromIntegral relations)
    
checkTables :: Manager -> VacationOpts -> IO ()
checkTables m VacationOpts{..} = phase "Checking tables" $ do
    undefined
    

main = do
    prog <- getProgName
    v@VacationOpts{..} <- cmdArgs (vacationOpts prog)

    setNumCapabilities clients

    r <- initRandom 0
    m <- initializeManager r relations
    cs <- initalizeClients m v

    as <- phase "Running clients" $ mapM (async . runClient) cs
    mapM_ wait as

    checkTables m v
