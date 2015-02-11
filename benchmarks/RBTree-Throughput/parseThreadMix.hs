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
    , _groups :: Maybe Int
    } deriving (Show, Eq)

makeLenses ''Opts

opts :: Parser Opts
opts = Opts <$> strArgument (help "File to parse") 
            <*> switch (long "format-Tikz" <> short 'T' <> help "output Tikz plot")
            <*> (optional . option auto) 
                (long "groups" <> short 'g' <> help "Tree operations per transaction")

grep :: String -> FilePath -> IO [String]
grep p f = lines <$> readProcess "grep" [p,f] ""

-- throughputAndSTMStat :: FilePath -> IO [(Double,Int)]
throughputAndSTMStat f = do
    ls <- lines <$> readFile f
    return $ process ls 
  where
    process [] = []
    process (l:ls) = let (s,ls') = stmStat ls in (l,s) : process ls'

--    stmStat [] = error "Expected STM stat entry!"
--    stmStat ls = (filter isDigit . head . words . last $ ss, skipBlanks 3 ls')
--      where
--        (ss,_:ls')  = break (=="") ls

    stmStat [] = error "Expected STM stat entry!"
    stmStat ls = (filter isDigit t, ls')
      where
        (t:ls') = skipBlanks 4 ls

    skipBlanks 0 ls = ls
    skipBlanks n ls = skipBlanks (n-1) (tail . snd . break (=="") $ ls)


main = do
    prog <- getProgName
    let p = info (helper <*> opts)
              (fullDesc <> progDesc "Parse benchmark run." <> header prog)
    opts <- execParser p

    -- get the times from perf's output
    ts <- throughputAndSTMStat (opts^.file)
    -- get the names of the executed command (assuming ./blah form)
    -- let xs = map (show . (/100) . fromIntegral) [9800..9999]
    let xs = map (show . (/1) . fromIntegral) [50..90]
    es <- (map (const "Mix") xs ++) 
        . mapMaybe (listToMaybe . words . drop 3 . dropWhile (/= '\''))
        <$> grep "Performance counter stats for" (opts^.file)


    if opts^.outputR
      then do
        let g = opts^.groups.non 1.to fromIntegral
        buildTabs es (xs ++ map (\(read -> r, read -> t) -> show (t*g/r)) ts) True
        buildPlot (S.toList . S.delete "Threads" . S.fromList $ es)
      else do
        buildTabs es (xs ++ map fst ts) False
        buildTabs es (xs ++ map snd ts) False
  where
    buildPlot es = do
        let hs = [ "\\begin{tikzpicture}[scale=2]"
                 , "\\begin{axis}["
                 , "    xlabel={Read Rate},"
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
            plot = ["\\addplot table[x=Mix,y=" 
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
