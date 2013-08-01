{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

-- import qualified KMeans as K
-- import Parse

import Random
import Manager

import Control.Monad
import Control.Applicative
import Control.Concurrent

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
    , relations    = 2^16
                  &= name "r"
                  &= help "Number of possible relations"
    , transactions = 2^26
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
        as = newListArray (0,l-1) is :: IOUArray Int Int

    -- Matching the biased shuffle from STAMP
    forM_ is $ \_ -> do
        (x,y) <- dup $ (`mod` l) <$> getRandom r
        vx <- readArray x
        vy <- readArray y
        writeArray x vy
        writeArray y vx

    getElems as

initializeManager :: Random -> Int -> IO Manager
initializeManager r relations = do
    putStrLn "Initalizing manager... "

    m <- mkManager

    let numTable = length managerAdd
        managerAdd = [ addCar
                     , addFlight
                     , addRoom
                     , addCustomer
                     ]

    forM_ managerAdd $ \add -> do
        is <- shuffle r [1..relations]
        
        forM_ is $ \i -> do
            num   <- (*100) . (+1) . (`mod` 5) <$> getRandom r
            price <- (+50) . (*10) . (`mod` 5) <$> getRandom r
            add m i num price

main = do
    prog <- getProgName
    VacationOpts{..} <- cmdArgs (vacationOpts prog)

    setNumCapabilities clients

    r <- initRandom 0

    m <- initializeManager r relations

    cs <- initalizeClients m

    putStrLn "Running clients... "

    runClients cs

    putStrLn "done."

    checkTables m
