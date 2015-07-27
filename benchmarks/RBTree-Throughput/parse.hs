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

data Mon = MMax | MMin | MAve | MMean
    deriving (Show, Eq, Bounded, Enum)

instance Read Mon where
    readsPrec _ r = case r `lookup` ms of
                      Just m -> [(m,[])]
                      Nothing -> []
      where
        os = [minBound..maxBound]
        ms = map (\m -> (show m, m)) os ++ map (\m -> (map toLower . tail . show $ m, m)) os

data Opts = Opts
    { _file :: FilePath
    , _outputR :: Bool
    , _xAxis  :: Maybe String
    , _fieldName :: Maybe String
    , _groups :: Maybe Int
    , _speedup :: Maybe Double
    , _monoid :: Mon
    , _files :: [FilePath]
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
            <*> (option auto)
                (value MMax <> long "monoid" <> short 'm' <> help "Combining operation")
            <*> (many $ strArgument (help "files for average"))

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

getData :: FilePath -> IO [[(String,String)]]
getData f = map (pairs . drop 1 . words) <$> grep "benchdata:" f
  where
    pairs :: [a] -> [(a,a)]
    pairs (a:b:cs) = (a,b) : pairs cs
    pairs _        = []

getValues f = do
    rs <- getData f

    let bs = map read $ mapMaybe (lookup "run-time") rs
        as = map read $ mapMaybe (lookup "transactions") rs

    return $ zipWith (/) as bs

overValues mf fs = do
    tss <- forM fs getValues
    return . map mf $ transpose tss

average fs = sum fs / (fromIntegral $ length fs)

mop MAve  = overValues average
mop MMax  = overValues maximum
mop MMin  = overValues minimum
mop MMean = overValues (\xs -> xs !! (length xs `div` 2))

main = do
    prog <- getProgName
    let p = info (helper <*> opts)
              (fullDesc <> progDesc "Parse benchmark run." <> header prog)
    opts <- execParser p

    let xn = opts^.xAxis.non "Threads"

    ts <- case opts^.files of
            [] -> getValues (opts^.file)
            fs -> mop (opts^.monoid) (opts^.file : fs)

    -- get the names of the executed command (assuming ./blah form)
    cs <- getData (opts^.file)

    let es' = mapMaybe (lookup "prog") cs
        esSet = S.fromList es'
        n  = S.size esSet
        xs' = mapMaybe (lookup (opts^.fieldName.non "threads")) cs
        xs = take (length xs' `div` n) xs'
        es = map (const xn) xs ++ es'



    if opts^.outputR
      then do
        case opts^.speedup of
          Just b -> do
            buildTabs es (xs ++ map (\t -> show (t/b)) ts) True 
            buildPlot True xn (S.toList esSet)
          Nothing -> do
            let g = opts^.groups.non 1.to fromIntegral
            buildTabs es (xs ++ map (\t -> show (t*g)) ts) True
            buildPlot False xn (S.toList esSet)
      else do
        buildTabs es (xs ++ map show ts) False
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
            name x = lookupContains
                        x [ ("IORef",         "Map")
                          , ("HashMap",       "HashMap")
                          , ("no-invariants", "STM-Fine")
                          , ("coarse",        "STM-Coarse")
                          , ("htm-bloom",     "Hybrid")
                          , ("hle-bloom",     "HTM-Coarse")
                          , ("fine-hle",      "HTM-Fine")
                          ] ^. non x
            out = unlines . concat $ [hs, plot, fs]
        writeFile "figures/throughput.tex" out

    lookupContains x ((k,v):es)
        | k `isInfixOf` x = Just v
        | otherwise       = lookupContains x es
    lookupContains _ [] = Nothing

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
