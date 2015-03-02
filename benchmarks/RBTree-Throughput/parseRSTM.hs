#!/bin/env runhaskell
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
    , _outputR :: Bool
    , _xAxis  :: Maybe String
    , _fieldName :: Maybe String
    , _groups :: Maybe Int
    , _speedup :: Maybe Double
    } deriving (Show, Eq)

makeLenses ''Opts

opts :: Parser Opts
opts = Opts <$> strArgument (help "File to parse")
            <*> switch (long "format-Tikz" <> short 'T' <> help "output Tikz plot")
            <*> (optional . option str)
                (long "x-axis" <> short 'x' <> help "x-axis label")
            <*> (optional . option str)
                (long "field-name" <> short 'f' <> help "x-axis field name")
            <*> (optional . option auto)
                (long "groups" <> short 'g' <> help "Tree operations per transaction")
            <*> (optional . option auto)
                (long "speedup" <> short 's' <> help "Plot speedup instead of throughput")

grep :: String -> FilePath -> IO [String]
grep p f = lines <$> readProcess "grep" [p,f] ""

fields :: String -> [(String, String)]
fields = map (pairs . splitOn "=")  . tail . splitOn ", "
  where
    pairs [a,b] = (a,b)
    pairs x = error $ "expected [a,b] saw " ++ show x

select :: String -> [[(String, String)]] -> [String]
select k rows = map (lookup' k) rows
  where
    lookup' k r = case lookup k r of
      Just v  -> v
      Nothing -> error $ "could not find key " ++ show k ++ " in row " ++ show r

main = do
    prog <- getProgName
    let p = info (helper <*> opts)
              (fullDesc <> progDesc "Parse benchmark run." <> header prog)
    opts <- execParser p

    let xn = opts^.xAxis.non "Threads"
        xf = "-" ++ opts^.fieldName.non "t"
    
    db <- map fields <$> grep "csv" (opts^.file)

    let ts    = select "throughput" db
        es'   = select "ALG" db
        esSet = S.fromList es'
        n     = S.size esSet
        xs'   = select "p" db
        xs    = take (length xs' `div` n) xs'
        es    = map (const xn) xs ++ es'

    if opts^.outputR
      then do
        case opts^.speedup of
          Just b -> do
            buildTabs es (xs ++ map (\(read -> t) -> show (t/b)) ts) True 
            buildPlot True xn (S.toList esSet)
          Nothing -> do
            buildTabs es (xs ++ ts) True
            buildPlot False xn (S.toList esSet)
      else do
        buildTabs es (xs ++ ts) False
  where
    buildPlot speedup xn es = do
        let hs = [ "\\begin{tikzpicture}[scale=2]"
                 , "\\begin{axis}["
                 , "    xlabel={" ++ xn ++ "},"
                 , if speedup
                     then "    ylabel={Speedup of tree operations per second},"
                     else "    ylabel={Tree operations per second},"
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
            plot = ["\\addplot table[x=" ++ xn ++ ",y=" 
                        ++ e 
                        ++ "] {\\loadedtable}; \\addlegendentry{"
                        ++ name e 
                        ++ "}"
                   | e <- es
                   ]
            name x = lookup x [ ("Main-no-invariants", "STM-Fine")
                              , ("Main-coarse", "STM-Coarse")
                              , ("Main-htm-bloom", "Hybrid")
                              , ("Main-hle-bloom", "HLE-Coarse")
                              , ("Main-IORef-no-invariants", "IORef")
                              ] ^. non x
            out = unlines . concat $ [hs, plot, fs]
        writeFile "figures/throughput.tex" out

    buildTabs es ts outputR = do
        let -- combine names with times, grouped by names
            ps = groupBy ((==) `on` fst) $ zip es ts
            -- pull out a single name, then join that as the first item of each list
            ss = map (uncurry (:)) . map ((fst . head) &&& map snd) $ ps
    
    
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
