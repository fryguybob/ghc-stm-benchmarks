{-# LANGUAGE ViewPatterns #-}
module Main where

import Runtracker.Database

import Options.Applicative

import System.Environment


data RuntrackerOptions = RuntrackerOptions
    { _add    :: Maybe FilePath
    , _create :: Maybe FilePath
    }

opts :: Parser RuntrackerOptinos
opts = RuntrackerOptions
    <$> (optional . strOption) (long "add"    <> short 'a' <> help "Add a log to the run database.")
    <*> (optional . strOption) (long "create" <> short 'c' <> help "Create a new empty run database.")

main :: IO ()
main = do
    prog <- getProgName
    let p = info (helper <*> opts)
                 ( fullDesc
                 <> progDec "Benchmark run tracking."
                 <> header prog)
    execParser opts >>= mainRuntracker

mainRunTracker :: RuntrackerOptions -> IO ()
mainRunTracker (RunTrackerOptions (Just a) 

