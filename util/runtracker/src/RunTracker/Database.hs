{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}

module RunTracker.Database where

import Control.Applicative
import Control.Lens
import Control.Lens.TH

import Data.Time

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- The particular benchmark run.
data Test = Test
    { _testKey        :: Int
    , _testName       :: String
    , _testCommitHash :: String
    } deriving (Show,Eq)

makeFields ''Test

instance FromRow Test where
    fromRow = Test <$> field <*> field <*> field

-- The compiler and configuration used.
data Config = Config
    { _configKey        :: Int
    , _configName       :: String
    , _configCommitHash :: String
    } deriving (Show,Eq)

makeFields ''Config

instance FromRow Config where
    fromRow = Config <$> field <*> field <*> field

-- The machine run on.
data Machine = Machine
    { _machineKey  :: Int
    , _machineName :: String
    } deriving (Show,Eq)

makeFields ''Machine

instance FromRow Machine where
    fromRow = Machine <$> field <*> field

-- The data from a run.
data Run = Run
    { _runKey       :: Int

    , _runCmdLine   :: String
    , _runDate      :: UTCTime
    , _runThreads   :: Int

    , _runTestId    :: Int
    , _runConfigId  :: Int
    , _runStatsId   :: Int
    , _runMachineId :: Int
    } deriving (Show,Eq)

makeFields ''Run

instance FromRow Run where
    fromRow = Run <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- General stats common to all runs
data Stats = Stats
    { _statsKey         :: Int
    , _statsRuntime     :: Double
    , _statsTestRuntime :: Double
    } deriving (Show,Eq)

makeFields ''Stats

instance FromRow Stats where
    fromRow = Stats <$> field <*> field <*> field

-- STM specific stats
data StatsSTM = StatsSTM 
    { _statsSTMKey          :: Int
    , _statsSTMRunId        :: Int

    , _statsSTMCap          :: Int
    , _statsSTMStarts       :: Int
    , _statsSTMAborts       :: Int
    , _statsSTMFailedWakeup :: Int
    } deriving (Show,Eq)

makeFields ''StatsSTM

instance FromRow StatsSTM where
    fromRow = StatsSTM <$> field <*> field <*> field <*> field <*> field <*> field

-- HTM specific stats
data StatsHTM = StatsHTM
    { _statsHTMKey      :: Int
    , _statsHTMRunId    :: Int

    , _statsHTMStarts   :: Int
    , _statsHTMCapacity :: Int
    , _statsHTMConflict :: Int
    } deriving (Show,Eq)

makeFields ''StatsHTM

instance FromRow StatsHTM where
    fromRow = StatsHTM <$> field <*> field <*> field <*> field <*> field


