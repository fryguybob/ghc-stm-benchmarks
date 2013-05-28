{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Parse
    ( parseFile
    ) where

import Control.Monad
import Control.Applicative

import System.IO

parseFile :: FilePath -> IO [[Float]]
parseFile f = map (map read . tail . words) . lines <$> readFile f
