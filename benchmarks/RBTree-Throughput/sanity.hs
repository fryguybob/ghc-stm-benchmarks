#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens hiding (argument)
import Control.Monad

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord
import qualified Data.Set as S

import Options.Applicative

import System.Directory
import System.Environment
import System.Process
import System.IO

import qualified Text.PrettyPrint.Boxes as B

import ParseLog
import Names

data Opts = Opts
    { _file       :: FilePath
    } deriving (Show, Eq)

makeLenses ''Opts

opts :: Parser Opts
opts = Opts <$> strArgument (help "File to parse")
            
getData :: FilePath -> IO [Run]
getData f = testParserFile runs f

getBenchCode f = do
    rs <- getData f

    let cs = mapMaybe (benchQuery "code") rs
        ps = mapMaybe (benchQuery "prog") rs 
    return $ zipWith (\c p -> [c,p]) cs ps

main = do
    prog <- getProgName
    let p = info (helper <*> opts)
              (fullDesc <> progDesc "Benchmark sanity check." <> header prog)
    opts <- execParser p

    -- get the names of the executed command (assuming ./blah form)
    bs <- getBenchCode (opts^.file)

    let csSet = S.fromList bs
        n     = S.size csSet
        [cs,ps] = transpose . S.elems $ csSet
        (t,ns) = nameSet ps
        out   = B.hsep 1 B.top . map (B.vcat B.left . map B.text) $ ["Code":cs,"Label":ns,"Exe":ps]
    
    putStrLn $ "Benchmarks: " ++ show n ++ " " ++ t
    B.printBox out
