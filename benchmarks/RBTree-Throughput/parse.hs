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
    { _file       :: FilePath
    , _outputR    :: Bool
    , _xAxis      :: Maybe String
    , _yAxis      :: Maybe String
    , _xFieldName :: Maybe String
    , _yFieldName :: Maybe String
    , _groups     :: Maybe Int
    , _speedup    :: Maybe Double
    , _logx       :: Bool
    , _monoid     :: Mon
    , _files      :: [FilePath]
    } deriving (Show, Eq)

makeLenses ''Opts

opts :: Parser Opts
opts = Opts <$> strArgument (help "File to parse")
            <*> switch (long "format-Tikz" <> short 'T' <> help "output Tikz plot")
            <*> (optional . option str)
                (long "x-axis" <> short 'x' <> help "x-axis label")
            <*> (optional . option str)
                (long "y-axis" <> short 'y' <> help "y-axis label")
            <*> (optional . option str)
                (long "x-field-name" <> short 'X' <> help "x-axis field name")
            <*> (optional . option str)
                (long "y-field-name" <> short 'Y' <> help "y-axis field name")
            <*> (optional . option auto)
                (long "groups" <> short 'g' <> help "Tree operations per transaction")
            <*> (optional . option auto)
                (long "speedup" <> short 's' <> help "Plot speedup instead of throughput")
            <*> switch (long "log-x" <> short 'l' <> help "log-scale x-axis")
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

{-
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
-}

getData :: FilePath -> IO [Run]
getData f = testParserFile runs f

getValues y f = do
    rs <- getData f

    let bs = map read $ mapMaybe (benchQuery "run-time") rs
        as = map read $ mapMaybe (fieldQuery y) rs

    return $ zipWith (/) as bs

fieldQuery k
    | "perf-" `isPrefixOf` k = fmap show . perfQuery (drop 5 k)
    | "cmd-"  `isPrefixOf` k = cmdLineQuery (drop 4 k)
    | otherwise              = benchQuery k

overValues y mf fs = do
    tss <- forM fs (getValues y)
    return . map mf $ transpose tss

average fs = sum fs / (fromIntegral $ length fs)

mop y MAve    = overValues y average
mop y MMax    = overValues y maximum
mop y MMin    = overValues y minimum
mop y MMedian = overValues y (\xs -> xs !! (length xs `div` 2))

sharedPrefix ::  Eq a => [[a]] -> [a]
sharedPrefix [] = []
sharedPrefix s = foldr1 sp2 s
  where
      sp2 l1 l2 = map fst . takeWhile (uncurry (==)) $ zip l1 l2

commonPrefix :: Eq a => [[a]] -> ([a], [[a]])
commonPrefix ss = (p, map (drop (length p)) ss)
  where
    p = sharedPrefix ss

trim :: Eq a => a -> [a] -> [a]
trim c = f . f
  where
    f = reverse . dropWhile (== c)

main = do
    prog <- getProgName
    let p = info (helper <*> opts)
              (fullDesc <> progDesc "Parse benchmark run." <> header prog)
    opts <- execParser p

    let xn = opts^.xAxis.non "Threads"
        yn = opts^.yAxis.non "Tree operations per second"
        y  = opts^.yFieldName.non "transactions"

    ts <- case opts^.files of
            [] -> getValues y (opts^.file)
            fs -> mop y (opts^.monoid) (opts^.file : fs)

    -- get the names of the executed command (assuming ./blah form)
    cs <- getData (opts^.file)

    let es' = mapMaybe (benchQuery "prog") cs
        esSet = S.fromList es'
        n  = S.size esSet
        xs' = mapMaybe (fieldQuery (opts^.xFieldName.non "threads")) cs
        xs = take (length xs' `div` n) xs'
        es = map (const xn) xs ++ es'

    if opts^.outputR
      then do
        case opts^.speedup of
          Just b -> do
            buildTabs es (xs ++ map (\t -> show (t/b)) ts) True 
            buildPlot True (opts^.logx) xn yn (S.toList esSet)
          Nothing -> do
            let g = opts^.groups.non 1.to fromIntegral
            buildTabs es (xs ++ map (\t -> show (t*g)) ts) True
            buildPlot False (opts^.logx) xn yn (S.toList esSet)
      else do
        buildTabs es (xs ++ map show ts) False
  where
    buildPlot speedup logx xn yn es = do
        let hs = concat
               [  [ "\\begin{tikzpicture}[scale=2]"
                  , "\\begin{axis}["
                  , "    ymin=0,"
                  ]
               ,  if logx
                    then ["    xmode=log,","    log ticks with fixed point,"]
                     else []
               , [ "    xlabel={" ++ xn ++ "},"
                 , if speedup
                     then "    ylabel={Speedup of " ++ yn ++ "},"
                     else "    ylabel={" ++ yn ++ "},"
                 , "    legend style={at={(0.5,-0.15)},anchor=north,legend columns=2},"
                 , "    title={" ++ trim '-' title ++ "},"
                 , "    cycle list name=my black white"
                 , "]"
                 , ""
                 , "\\pgfplotstableread{throughput.dat}\\loadedtable"
                 , ""
                 ]
               ]
            fs = [ ""
                 , "\\end{axis}"
                 , "\\end{tikzpicture}"
                 ]
            (title,names) = commonPrefix (map (trim '-' . trim '8'. name) es)
            plot = ["\\addplot table[x=" ++ xn ++ ",y=" 
                        ++ e 
                        ++ "] {\\loadedtable}; \\addlegendentry{"
                        ++ n
                        ++ "}"
                   | (n,e) <- zip names es
                   ]
            name x = lookupContains
                        x [ ("cuckoo-tstruct-int-fine", "Cuckoo-TStruct")
                          , ("cuckoo-tstruct-fine", "Cuckoo-TStruct-k")
                          , ("cuckoo-tvar-fine-simple", "Cuckoo-TVar-Simple")
                          , ("cuckoo-tvar-fine", "Cuckoo-TVar")

                          , ("rbtreemutstm",  "RBTree-STM-mut")
                          , ("rbtree-TStruct", "RBTree-STM-TStruct")
                          , ("rbtree-TVar",   "RBTree-STM-TVar")

                          , ("rbtreeioref",     "RBTree-IORef")
                          , ("rbtreemutsingle", "RBTree-mut-single")
                          , ("IORef",         "Map")
                          , ("HashMap",       "HashMap")
                          , ("no-invariants", "RBTree-Fine")
                          , ("coarse",        "STM-Coarse")
                          , ("htm-bloom",     "Hybrid")
                          , ("hle-bloom",     "HTM-Coarse")
                          , ("fine-hle",      "HTM-Fine")

                          , ("rbtree-tstruct-fine", "RBTree-TStruct-STM")
                          , ("rbtree", "RBTree-STM")

                          , ("skiplist-tstruct-fine", "Skiplist-TStruct")
                          , ("skiplist-tstruct", "Skiplist-TStruct-Hybrid")
                          , ("skiplist",      "Skiplist-TVar")

                          , ("stmtrie-tstruct-fine-old", "HAMT-TStruct-STM-old")
                          , ("stmtrie-tstruct-fine-htm", "HAMT-TStruct-fine-HTM")
                          , ("stmtrie-tstruct-fine", "HAMT-TStruct-STM")
                          , ("stmtrie-fine-htm", "HAMT-TVar-Fine-HTM")
                          , ("stmtrie-fine",  "HAMT-Fine")

                          , ("tstruct-fine-htm",  "RBTree-TStruct-Fine-HTM")
                          , ("tstruct-fine",  "RBTree-TStruct-STM")
                          , ("tstruct",       "RBTree-TStruct-Hybrid")

                          , ("fine-htm",      "RBTree-TVar-Fine-HTM")

                          ] ^. non x
            out = unlines . concat $ [hs, plot, fs]
        writeFile "figures/throughput.tex" out

    lookupContains x ((k,v):es)
        | k `isInfixOf` x = Just (keepSuffix x v)
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

keepSuffix x v =
    case suffix x of
      Just s  -> v ++ s
      Nothing -> v

suffix x
   | length s > 0 = Just $ reverse s
   | otherwise    = Nothing
  where
    s = takeWhile digitOrDash . reverse $ x
    digitOrDash c = (c >= '0' && c <= '9') || c == '-'


