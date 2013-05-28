{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import qualified KMeans as K
import Parse

import Control.Monad
import Control.Applicative
import Control.Concurrent

import System.Environment (getProgName)
import System.Directory (doesFileExist)

import System.Console.CmdArgs.Implicit

data KMeansOpts = KMeansOpts
    { filename     :: FilePath
    , binaryFormat :: Bool
    , maxClusters  :: Int
    , minClusters  :: Int
    , notZTrans    :: Bool
    , threshold    :: Float
    , threads      :: Int
    } deriving (Show, Data, Typeable)

kmeansOpts prog = KMeansOpts
    { filename     = def 
                  &= typFile
                  &= name "i"
                  &= help "file containing data to be clustered"
    , binaryFormat = False
                  &= name "b"
                  &= help "input file is in binary format"
    , maxClusters  = 13
                  &= name "m"
                  &= help "maximum number of clusters allowed"
    , minClusters  = 4
                  &= name "n"
                  &= help "minimum number of clusters allowed"
    , notZTrans    = False
                  &= explicit
                  &= name "z"
                  &= help "don't zscore transform data"
    , threshold    = 0.001
                  &= name "t"
                  &= help "threshold value"
    , threads      = 1
                  &= name "p"
                  &= help "number of threads"
    }
    &= program prog

main = do
    prog <- getProgName
    KMeansOpts{..} <- cmdArgs (kmeansOpts prog)

    when (filename == "") $ fail "Error: input file must be specified."
    e <- doesFileExist filename
    when (not e) $ fail "Error: input file not found."
    when (minClusters > maxClusters) $ fail "Error: max_clusters must be >= min_clusters."

    setNumCapabilities threads

    when (binaryFormat) $ fail "Error: binary format feature not implemented."

    ds <- parseFile filename

    cs <- K.cluster (K.Config threads (not notZTrans) minClusters maxClusters threshold) ds
    print cs
