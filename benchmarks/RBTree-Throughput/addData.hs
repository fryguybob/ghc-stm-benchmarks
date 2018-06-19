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

data Mon = MMax | MMin | MAve | MMedian
    deriving (Show, Eq, Bounded, Enum)

instance Read Mon where
    readsPrec _ r = case r `lookup` ms of
                      Just m -> [(m,[])]
                      Nothing -> []
      where
        os = [minBound..maxBound]
        ms = map (\m -> (show m, m)) os ++ map (\m -> (map toLower . tail . show $ m, m)) os

data Opts = Opts
    { _monoid     :: Mon
    , _name       :: String
    , _prefix     :: String
    , _desc       :: String
    , _heapData   :: Bool
    } deriving (Show, Eq)

makeLenses ''Opts

opts :: Parser Opts
opts = Opts <$> (option auto)
                (value MMax <> long "monoid" <> short 'm' <> help "Combining operation")
            <*> strArgument (help "Index entry name")
            <*> strArgument (help "Prefix for log files")
            <*> strArgument (help "Description")
            <*> switch (long "heap" <> short 'H' <> help "plot heap data")

getData :: FilePath -> IO [Run]
getData f = testParserFile runs f

getValuesHeap y f = do
    rs <- getData f
    return $ map (fromIntegral . head . fromJust' "heap query" . heapQuery y) rs
  where
    fromJust' _ (Just a) = a
    fromJust' s _        = error s

getValuesThroughput y f = do
    rs <- getData f

    let bs = map read $ mapMaybe (benchQuery "run-time") rs
        as = map read $ mapMaybe (fieldQuery y) rs

    return $ zipWith (/) as bs

getValues :: Bool -> String -> FilePath -> IO [Double]
getValues True  y f = getValuesHeap y f
getValues False y f = getValuesThroughput y f

fieldQuery k
    | "perf-" `isPrefixOf` k = fmap show . perfQuery (drop 5 k)
    | "cmd-"  `isPrefixOf` k = cmdLineQuery (drop 4 k)
    | otherwise              = benchQuery k

overValues heap y mf fs = do
    tss <- forM fs (getValues heap y)
    return . map mf $ transpose tss

average fs = sum fs / (fromIntegral $ length fs)

mop heap y MAve    = overValues heap y average
mop heap y MMax    = overValues heap y maximum
mop heap y MMin    = overValues heap y minimum
mop heap y MMedian = overValues heap y (\xs -> xs !! (length xs `div` 2))

main = do
    prog <- getProgName
    let p = info (helper <*> opts)
              (fullDesc <> progDesc "Add data to data explorer." <> header prog)
        xn = "Threads"
        y = "transactions"
    opts <- execParser p

    print opts

    files@(file:_) <- map ("logs/" ++) . filter ((opts^.prefix) `isPrefixOf`)
                        <$> getDirectoryContents "logs/"
    print files

    ts <- mop (opts^.heapData) y (opts^.monoid) files :: IO [Double]

    -- get the names of the executed command (assuming ./blah form)
    cs <- getData file

    let es' = mapMaybe (benchQuery "prog") cs
        esSet = S.fromList es'
        n  = S.size esSet
        xs' = mapMaybe (fieldQuery "threads") cs
        xs = take (length xs' `div` n) xs'
        es = map (const xn) xs ++ es'
        (title,names) = nameSet es'

        y0 = maximum . map (head . map snd) . groupBy ((==) `on` fst) $ zip es ts

    buildTabs (opts^.prefix) (map (const xn) xs ++ names) (xs ++ map (\t -> show t) ts) True
    buildIndex (title ++ " " ++ (opts^.name)) (opts^.desc) (opts^.prefix) (maximum ts) y0
  where

    buildIndex :: String -> String -> String -> Double -> Double -> IO ()
    buildIndex name desc prefix maxY y0 = do
        let json = concat [ "{ "
                          , "\"name\": \"", name,        "\", "
                          , "\"url\": \"" , prefix,      ".dat\", "
                          , "\"maxY\": "  , show maxY,   ", "
                          , "\"y0\": "    , show y0,     ", "
                          , "\"description\": \"", desc, "\""
                          , "},"
                          ]
        appendFile "html/index.dat" json
        putStrLn json
        -- { "name": "...", "url": "....dat", "maxY": ..., "y0": ..., "description": "..." },

    buildTabs prefix es ts outputR = do
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
        
        writeFile ("html/" ++ prefix ++ ".dat") (B.render bs)
        B.printBox bs
