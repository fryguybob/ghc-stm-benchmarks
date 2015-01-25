{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}

module RunTracker.Database where

import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Lens.TH

import Data.Default.Class
import Data.Int
import Data.Time
import Data.String

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField

import RunTracker.Util

import System.IO
import System.Process
import System.Directory
import System.Exit
import System.FilePath

import Paths_runtracker


justField = Just <$> field

-- The particular benchmark run.
data Test = Test
    { _testKey        :: Maybe Int64
    , _testName       :: String
    , _testCommitHash :: String
    } deriving (Show,Eq)

makeFields ''Test

instance Default Test where
    def = Test Nothing "" ""

instance FromRow Test where
    fromRow = Test <$> justField <*> field <*> field

-- The compiler and configuration used.
data Config = Config
    { _configKey        :: Maybe Int64
    , _configName       :: String
    , _configCommitHash :: String
    } deriving (Show,Eq)

makeFields ''Config

instance Default Config where
    def = Config Nothing "" ""

instance FromRow Config where
    fromRow = Config <$> justField <*> field <*> field

-- The machine run on.
data Machine = Machine
    { _machineKey  :: Maybe Int64
    , _machineName :: String
    } deriving (Show,Eq)

makeFields ''Machine

instance Default Machine where
    def = Machine Nothing ""

instance FromRow Machine where
    fromRow = Machine <$> justField <*> field

-- The data from a run.
data Run = Run
    { _runKey       :: Maybe Int64

    , _runCmdLine   :: String
    , _runDate      :: UTCTime
    , _runThreads   :: Int

    , _runTestId    :: Int64
    , _runConfigId  :: Int64
    , _runStatsId   :: Int64
    , _runMachineId :: Int64
    } deriving (Show,Eq)

makeFields ''Run

instance Default Run where
    def = Run Nothing "" (UTCTime (ModifiedJulianDay 0) 0) 0  0 0 0 0

instance FromRow Run where
    fromRow = Run <$> justField <*> field <*> field <*> field 
                  <*> field <*> field <*> field <*> field

-- General stats common to all runs
data Stats = Stats
    { _statsKey         :: Maybe Int64
    , _statsRuntime     :: Double
    , _statsTestRuntime :: Double
    } deriving (Show,Eq)

makeFields ''Stats

instance Default Stats where
    def = Stats Nothing 0 0

instance FromRow Stats where
    fromRow = Stats <$> justField <*> field <*> field

-- STM specific stats
data StatsSTM = StatsSTM 
    { _statsSTMKey          :: Maybe Int64
    , _statsSTMRunId        :: Int64

    , _statsSTMCap          :: Int
    , _statsSTMStarts       :: Int
    , _statsSTMAborts       :: Int
    , _statsSTMFailedWakeup :: Int
    } deriving (Show,Eq)

makeFields ''StatsSTM

instance Default StatsSTM where
    def = StatsSTM Nothing 0  0 0 0 0

instance FromRow StatsSTM where
    fromRow = StatsSTM <$> justField <*> field <*> field <*> field <*> field <*> field

-- HTM specific stats
data StatsHTM = StatsHTM
    { _statsHTMKey      :: Maybe Int64
    , _statsHTMRunId    :: Int64

    , _statsHTMStarts   :: Int
    , _statsHTMCapacity :: Int
    , _statsHTMConflict :: Int
    } deriving (Show,Eq)

makeFields ''StatsHTM

instance Default StatsHTM where
    def = StatsHTM Nothing 0  0 0 0

instance FromRow StatsHTM where
    fromRow = StatsHTM <$> justField <*> field <*> field <*> field <*> field

createDatabase :: FilePath -> IO ()
createDatabase d = do
    d' <- sane d
    b <- doesFileExist d'
    whenM (doesFileExist d') $ error $ "File '" ++ d' ++ "' already exists."

    info $ "Creating database '" ++ d' ++ "'"

    f <- getDataFileName "mkdb.sql"

    r <- systemEffectfully $ "sqlite3 '" ++ d' ++ "' < '" ++ f ++ "'"
    when (r /= ExitSuccess) $ print r
  where
    sane f = do
        let (d,n) = splitFileName f
        d' <- canonicalizePath d
        return $ d' </> normalise f
        -- TODO: Is this enough?

addMachine :: FilePath -> String -> IO ()
addMachine d n = do
    conn <- open d
    execute conn "INSERT INTO machines (name) VALUES (?)" (Only n)

addConfig :: FilePath -> String -> String -> IO ()
addConfig d n h = do
    conn <- open d
    execute conn "INSERT INTO configs (name, commitHash) VALUES (?, ?)" (n,h)

addTest :: FilePath -> String -> String -> IO ()
addTest d n h = do
    conn <- open d
    execute conn "INSERT INTO tests (name, commitHash) VALUES (?, ?)" (n,h)
    
addRun :: FilePath -> Run -> IO ()
addRun d r = do
    conn <- open d
    execute conn "INSERT INTO runs (cmdLine, date, threads, testid, configid, statsid, machineid) VALUES (?,?,?,?,?,?)"
       (r^.cmdLine, r^.date, r^.threads, r^.testId, r^.configId, r^.statsId, r^.machineId)

addRuns :: FilePath -> [(Run, Stats, Maybe StatsHTM, [StatsSTM])] -> IO ()
addRuns d rs = do
    conn <- open d
    forM_ rs $ \(r, s, mh, ss) -> do
        execute conn "INSERT INTO stats (runtime,testRuntime) VALUES (?, ?)"
                     (s^.runtime, s^.testRuntime)
        sId <- lastInsertRowId conn
        insertRun conn (r & statsId .~ sId)
        rId <- lastInsertRowId conn
        maybe (return ()) (insertHTM conn) (mh <&> (runId .~ rId))
        forM_ ss $ insertSTM conn . (runId.~rId)
  where
    insertRun conn r = do
        execute conn "INSERT INTO runs\
                     \ (cmdLine, date, threads, testid, configid, statsid, machineid)\
                     \ VALUES (?,?,?,?,?,?)"
                     ( r^.cmdLine, r^.date, r^.threads
                     , r^.testId, r^.configId, r^.statsId, r^.machineId
                     )

    insertHTM conn h = do
        execute conn "INSERT INTO statsHTM (runid,starts,capacity,conflict) VALUES (?,?,?,?)"
                        (h^.runId, h^.starts, h^.capacity, h^.conflict)

    insertSTM conn s = do
        execute conn "INSERT INTO statsSTM (runid,cap,starts,aborts,failedWakeup) VALUES (?,?,?,?)"
                        (s^.runId, s^.cap, s^.starts, s^.aborts, s^.failedWakeup)
 
queryDatabase :: FromRow a => FilePath -> String -> IO [a]
queryDatabase d q = do
    info $ "Querying database '" ++ d ++ "'"
    info $ "Query '" ++ q ++ "'"

    conn <- open d
    query_ conn (fromString q) -- TODO: what to do with the query?

