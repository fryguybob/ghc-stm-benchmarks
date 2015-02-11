#!/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens hiding (argument)
import Control.Monad

import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import qualified Data.Set as S

import Options.Applicative

import System.Directory
import System.Environment
import System.Process
import System.IO

import qualified Text.PrettyPrint.Boxes as B

data Opts = Opts
    { _file :: FilePath
    } deriving (Show, Eq)

makeLenses ''Opts

opts :: Parser Opts
opts = Opts <$> strArgument (help "File to parse")


grep :: String -> FilePath -> IO [String]
grep p f = lines <$> readProcess "grep" [p,f] ""


main = do
    prog <- getProgName
    let p = info (helper <*> opts)
              (fullDesc <> progDesc "Parse benchmark run." <> header prog)
    opts <- execParser p

    -- get the times from perf's output
    ts <- mapMaybe (listToMaybe . words) <$> grep "seconds" (opts^.file)
    -- get the names of the executed command (assuming ./blah form)
    es <- mapMaybe (listToMaybe . words . drop 3 . dropWhile (/= '\''))
        <$> grep "Performance counter stats for" (opts^.file)

    let -- combine names with times, grouped by names
        ps = groupBy ((==) `on` fst) $ zip es ts
        -- pull out a single name, then join that as the first item of each list
        ss = map (uncurry (:)) . map ((fst . head) &&& map snd) $ ps


        tabs = map (concat . intersperse "\t") . transpose $ ss


        -- column of commas
        cs = B.vcat B.left . map (const (B.text ",")) . head $ ss
        -- Build comma separated table with boxes.
        bs = B.hsep 2 B.top . intersperse cs . map (B.vcat B.left . map B.text) $ ss

        -- Build table with boxes.
        -- bs = B.hsep 2 B.top . . map (B.vcat B.left . map B.text) $ ss

    -- B.printBox bs
    putStrLn . unlines $ tabs
