{-# LANGUAGE RecordWildCards #-}
module Main where

-- import Random
-- import RandomMWC
import RandomPCG
import Reservation
import Customer
import Manager
import Client

import Throughput

import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import System.IO
import System.Exit

import GHC.Conc

import Data.Array.IO

import System.Environment (getProgName)
import System.Directory (doesFileExist)

import Options.Applicative


data VacationOpts = VacationOpts
    { clients      :: Int
    , number       :: Int
    , queries      :: Int
    , relations    :: Int
    , transactions :: Int
    , user         :: Int
    , phases       :: Int
    , throughput   :: Maybe Int
    } deriving (Show)

vacationOpts :: Parser VacationOpts
vacationOpts = VacationOpts
    <$> (option auto)
        (value 1 <> long "clients" <> short 'c' <> help "Number of clients")
    <*> (option auto)
        (value 10 <> long "number" <> short 'n' <> help "number of user queries/transaction")
    <*> (option auto)
        (value 90 <> long "queries" <> short 'q' <> help "Percentage of relations queried")
    <*> (option auto)
        (value (2^16 :: Int) <> long "relations" <> short 'r' <> help "Number of possible relations")
    <*> (option auto)
        (value (2^26 :: Int) <> long "transactions" <> short 't' <> help "Number of transactions")
    <*> (option auto)
        (value 80 <> long "user" <> short 'u' <> help "Percentage of user transactions")
    <*> (option auto)
        (value 3 <> long "phases" <> short 'p' <> help "Number of phases, init, run, check")
    <*> (optional . option auto)
        (long "throughput" <> short 's' <> help "Throughput runtime in milliseconds")

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
        , "    Throughput          = " ++ show throughput
        ]
    return cs
  where
    ts = case throughput of
           Just _  -> -1 
           Nothing -> round (fromIntegral transactions / fromIntegral clients)
    range = round (fromIntegral queries / 100.0 * fromIntegral relations)
    
checkTables :: Manager -> VacationOpts -> IO ()
checkTables m VacationOpts{..} = phase "Checking tables" $ do
    checkUniqueCustomers m (round (fromIntegral queries * fromIntegral relations / 100) + 1)
    checkUniqueTables m relations


guard' False = exitSuccess
guard' True  = return ()

main = do
    prog <- getProgName
    let p = info (helper <*> vacationOpts)
                (fullDesc <> progDesc "Vacation benchmark." <> header prog)
    v@VacationOpts{..} <- execParser p

    guard' (phases > 0)

    setNumCapabilities clients

    r <- initRandom 0
    m <- initializeManager r relations


    cs <- initalizeClients m v

    case throughput of
      Just s -> do
        guard' (phases > 1)
        (t,ta) <- throughputMain (s * 1000) (map runClient cs)
        trans <- sum <$> forM cs countClient
        putStrLn $ unwords [ "benchdata:"
                           , "run-time"    , show t
                           , "no-kill-time", show ta
                           , "transactions", show trans
                           , "prog"        , prog
                           , "threads"     , show clients
                           ] 
      Nothing -> do
        as <- forM cs $ \c -> do
            l <- newEmptyMVar
            forkIO (when (phases > 1) (runClient c) >> putMVar l ())
            return l
        forM_ as takeMVar
    
    guard' (phases > 2)
    
    checkTables m v

