#!/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
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
    , _initFile :: FilePath
    , _outputR :: Bool
    } deriving (Show, Eq)

makeLenses ''Opts

opts :: Parser Opts
opts = Opts <$> strArgument (help "File to parse")
            <*> strArgument (help "Init file to parse")
            <*> switch (long "format-Tikz" <> short 'T' <> help "output Tikz plot")


grep :: String -> FilePath -> IO [String]
grep p f = lines <$> readProcess "grep" [p,f] ""

readDouble :: String -> Double
readDouble = read

main = do
    prog <- getProgName
    let p = info (helper <*> opts)
              (fullDesc <> progDesc "Parse benchmark run." <> header prog)
    opts <- execParser p

    is <- map readDouble . mapMaybe (listToMaybe . words) <$> grep "seconds" (opts^.initFile)
    -- get the times from perf's output
    ts <- map show . zipWith (\x y -> y - x) is 
        . map readDouble . mapMaybe (listToMaybe . words) <$> grep "seconds" (opts^.file)
    -- get the names of the executed command (assuming ./blah form)
    let xs = map show [1..72]
    es <- (map (const "Threads") xs ++)
        . mapMaybe (listToMaybe . words . drop 3 . dropWhile (/= '\''))
        <$> grep "Performance counter stats for" (opts^.file)

    if opts^.outputR
      then do
        buildTabs es (xs ++ ts) True
        buildPlot (S.toList . S.delete "Threads" . S.fromList $ es)
      else do
        buildTabs es (xs ++ ts) False
  where
     buildPlot es = do
        let hs = [ "\\begin{tikzpicture}[scale=2]"
                 , "\\begin{axis}["
                 , "    xlabel={Threads},"
                 , "    ylabel={Tree operations per second},"
                 , "    legend style={at={(0.5,-0.15)},anchor=north,legend columns=-1}"
                 , "]"
                 , ""
                 , "\\pgfplotstableread{throughput.dat}\\loadedtable"
                 , ""
                 ]
            fs = [ ""
                 , "\\end{axis}"
                 , "\\end{tikzpicture}"
                 ]
            plot = ["\\addplot table[x=Threads,y="
                        ++ name e
                        ++ "] {\\loadedtable}; \\addlegendentry{"
                        ++ name e
                        ++ "}"
                   | e <- es
                   ]
            out = unlines . concat $ [hs, plot, fs]
        writeFile "figures/throughput.tex" out

     name x = lookup x [ (".cabal-sandbox-no-invariants/bin/vacation", "STM-Fine")
                       , (".cabal-sandbox-coarse/bin/vacation",        "STM-Coarse")
                       , (".cabal-sandbox-htm-bloom/bin/vacation",     "Hybrid")
                       , (".cabal-sandbox-hle-bloom/bin/vacation",     "HLE-Coarse")
                       ] ^. non x
     buildTabs es ts outputR = do
        let -- combine names with times, grouped by names
            ps = groupBy ((==) `on` fst) $ zip es ts
            -- pull out a single name, then join that as the first item of each list
            ss = map (uncurry (:)) . map ((name . fst . head) &&& map snd) $ ps

            tabs = map (concat . intersperse "\t") . transpose $ ss

            -- column of commas
            cs = B.vcat B.left . map (const (B.text ",")) . head $ ss
            -- Build comma separated table with boxes.
            bsComma = B.hsep 2 B.top . intersperse cs . map (B.vcat B.left . map B.text) $ ss

            -- Build table with boxes.
            bs = B.hsep 2 B.top . map (B.vcat B.left . map B.text) $ ss

        if outputR
          then writeFile "figures/throughput.dat" (B.render bs)
          else B.printBox bs
        -- putStrLn . unlines $ tabs
