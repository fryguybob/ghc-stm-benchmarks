#!/bin/env runhaskell
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
import Options.Applicative
import Control.Applicative
import Control.Monad
import Control.Lens hiding (argument)
import Control.Concurrent.MVar

import Data.List (isInfixOf, intercalate)
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Data.Maybe (isJust)

import System.Environment
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import System.Directory
import System.Exit

import GHC.IO.Handle (hDuplicate)

data Flavor = Flavor
    { _tstruct   :: Bool 
    , _fine      :: Bool
    , _hybrid    :: Bool
    , _htmCommit :: Bool
    }
    deriving (Eq)
makeLenses ''Flavor

readFlavor :: String -> Flavor
readFlavor (map toLower -> s) =
    Flavor ("tstruct" `isInfixOf` s)
           ("fine"    `isInfixOf` s)
           ("hybrid"  `isInfixOf` s)
           ("htm"     `isInfixOf` s)

mwhen False _ = mempty
mwhen True  a = pure a

showFlavor :: Flavor -> String
showFlavor (Flavor t f h c)
  = intercalate "-" ( catMaybes [ Just $ if t then "TStruct" else "TVar"
                                , Just $ if f then "fine"    else "coarse"
                                , mwhen h "hybrid"
                                , mwhen c "htm"
                                ])

hasHtm :: Flavor -> Bool
hasHtm (Flavor _ _ a b) = a || b

instance Show Flavor where
  show f = "readFlavor \"" ++ showFlavor f ++ "\""

prop_flavorReadShow f = f == (readFlavor . showFlavor $ f)

type ScriptHandler = String -> IO ()

instance Show ScriptHandler where
  show _ = "<to-script>"


data BuildOpts = BuildOpts
    { _buildOptsBenchmark :: String   -- Benchmark is the data-structure we are going to
                                      -- test.  This determines the flags and extra
                                      -- libraries we will need to install.
    , _buildOptsFlavors   :: [Flavor] -- flavor is the kind of TM we are targeting.  This
                                      -- determines the compiler we will use and some
                                      -- compile flags.
    , _buildOptsDebugDump :: Bool
    , _buildOptsDryRun    :: Bool
    , _buildOptsScript    :: Maybe ScriptHandler
    }
    deriving (Show)
makeFields ''BuildOpts

data CleanOpts = CleanOpts
    { _cleanOptsCleanAll :: Bool
    , _cleanOptsDryRun   :: Bool
    , _cleanOptsScript   :: Maybe ScriptHandler
    }
    deriving (Show)
makeFields ''CleanOpts

data RunOpts = RunOpts
    { _runOptsBenchmark     :: String
    , _runOptsFlavors       :: [Flavor]
    , _runOptsOutput        :: String
    , _runOptsHtmRetry      :: Maybe Int
    , _runOptsHleRetry      :: Maybe Int
    , _runOptsReadMix       :: Int
    , _runOptsMilliseconds  :: Int
    , _runOptsAffinity      :: String
    , _runOptsHeapStatAccum :: Maybe Int
    , _runOptsDryRun        :: Bool
    , _runOptsScript        :: Maybe ScriptHandler
    }
    deriving (Show)
makeFields ''RunOpts

data Cmd
    = Clean CleanOpts
    | Build BuildOpts
    | Run   RunOpts
    deriving (Show)
makeLenses ''Cmd

data Opts = Opts
    { _cmd :: Cmd
    }
    deriving (Show)
makeLenses ''Opts

type HasActOpts o = (HasDryRun o Bool, HasScript o (Maybe ScriptHandler))

opts :: Parser Opts
opts = Opts <$> subparser
    (  command "clean" (info (Clean <$> cleanOpts) (progDesc "Clean intermeidate files."))
    <> command "build" (info (Build <$> buildOpts) (progDesc "Build benchmark."))
    <> command "run"   (info (Run   <$> runOpts  ) (progDesc "Run benchmark.")))


cleanOpts :: Parser CleanOpts
cleanOpts = CleanOpts <$> switch (long "all" <> short 'a' <> help "Clean all files.")
                      <*> dryRunArg
                      <*> scriptArg

buildOpts :: Parser BuildOpts
buildOpts = BuildOpts <$> strArgument (help "Benchmark to build.")
                      <*> flavorsArg
                      <*> switch (long "core" <> short 'c' <> help "Dump core.")
                      <*> dryRunArg
                      <*> scriptArg

runOpts :: Parser RunOpts
runOpts = RunOpts <$> strArgument (help "Benchmark to run.")
                  <*> flavorsArg
                  <*> strOption (long "output" <> short 'o' <> help "output.")
                  <*> (optional . option auto) (long "htm-retry" <> help "htm-retry.")
                  <*> (optional . option auto) (long "hle-retry" <> help "hle-retry.")
                  <*> option auto (long "read-mix"  <> value 90 <> showDefault <> help "Read mix.")
                  <*> option auto (long "time"      <> value 1000 <> showDefault 
                                        <> help "Time for run in milliseconds.")
                  <*> option auto (long "affinity"  <> value "topo-cores-sockets-threads"
                                        <> showDefault <> help "Thread affinity map.")
                  <*> (optional . option auto) (long "stm-accum" <> help "stm-accum.")
                  <*> dryRunArg
                  <*> scriptArg

flavorsArg :: Parser [Flavor]
flavorsArg = map readFlavor . words <$> strArgument (help "Flavors")

dryRunArg :: Parser Bool
dryRunArg = switch (long "dry-run" <> short 'd' <> help "Dry run.")

scriptArg :: Parser (Maybe ScriptHandler)
scriptArg = (fmap buildScript) <$> (optional . strOption)
                (long "script" <> short 's' <> help "Output to script.")

globalFiles :: MVar (M.Map FilePath Handle)
globalFiles = unsafePerformIO $ newMVar M.empty
{-# NOINLINE globalFiles #-}

getFile :: FilePath -> IO Handle
getFile f = do
    m <- takeMVar globalFiles
    case M.lookup f m of
        Just h  -> putMVar globalFiles m >> return h
        Nothing -> do
            h <- openFile f WriteMode
            hPutStrLn h "#!/bin/sh"
            hPutStrLn h "# Auto generated by:"
            n <- getProgName
            as <- getArgs
            let s = unwords (n : map quote as)
            hPutStrLn h $ unlines $ zipWith (++) (repeat "# ") (lines s)
            putMVar globalFiles (M.insert f h m)
            return h
  where
    quote s
      | any (== ' ') s = "\"" ++ s ++ "\""
      | otherwise      = s

buildScript :: FilePath -> ScriptHandler 
buildScript f s = do
  h <- getFile f
  hPutStrLn h s

---------------------------------------------------------------------------------
act :: HasActOpts o => o -> String -> IO ()
act opts c
  | opts^.dryRun = putStrLn c
  | Just out <- opts^.script = out c
  | otherwise = putStrLn c >> systemFail c

systemFail :: String -> IO ()
systemFail c = system c >>= handleExit

handleExit :: ExitCode -> IO ()
handleExit ExitSuccess       = return ()
handleExit x@(ExitFailure n) = putStrLn ("Process exited with " ++ show n) >> exitWith x

launchMissles :: HasActOpts o => o -> Bool
launchMissles o = not (o^.dryRun) && not (isJust (o^.script))

echo :: HasActOpts o => o -> String -> IO ()
echo opts c
  | opts^.dryRun = putStrLn c
  | Just out <- opts^.script = out c
  | otherwise = putStrLn c

type Redirect = Maybe Handle

initRedirect :: (HasActOpts o, HasOutput o String) => o -> String -> IO Redirect
initRedirect o suffix
  | o^.output == "" || not (launchMissles o) = return Nothing
  | otherwise = Just <$> openFile
                  ("logs/" ++ o^.output
                    ++ (if suffix == "" 
                         then ""
                         else "-" ++ suffix)
                    ++ ".log") WriteMode

closeRedirect :: HasActOpts o => o -> Redirect -> IO ()
closeRedirect o (Just h)
    | launchMissles o = hClose h
closeRedirect _ _     = return ()

actRedirect :: HasActOpts o => o -> Redirect -> String -> IO ()
actRedirect o (Just h) c
    | launchMissles o = do
         putStrLn c
         h' <- hDuplicate h
         (_,_,_,p) <- createProcess (shell c)
                        { delegate_ctlc = True
                        , std_out = UseHandle h'
                        , std_err = UseHandle h'
                        }
         waitForProcess p >>= handleExit
actRedirect o _ c = act o c

---------------------------------------------------------------------------------
-- Build

build :: BuildOpts -> IO ()
build opts = forM_ (opts^.flavors) $ \f -> do
                echo opts ("# " ++ n f)
                act  opts (sandbox f)
                act  opts (cabal f)
                act  opts (copy f)
                echo opts ""
  where
    sb f = ".cabal-sandbox-" ++ intercalate "-" [opts^.benchmark, showFlavor f] ++ "-8"
    n f = opts^.benchmark ++ "-" ++ showFlavor f
    sandbox f = unwords'
        [ "cabal sandbox init" 
        , "--sandbox=" ++ sb f
        ]
    dump f
      | opts^.debugDump = "-ddump-simpl -ddump-cmm -ddump-to-file -dumpdir dump/" ++ n f
      | otherwise       = ""

    cabal f = unwords'
        [ "cabal install"
        , "optparse-applicative"
        , "-f-bytecounter"
        , "../throughput/"
        , "../random/pcg-random/"
        , "./"
        , flags opts f
        , dump f
        , "--with-ghc " ++ ghc f
        , "--ghc-options=\"-O2 -msse4.2\"" -- This is for all the other libraries
        ]

    copy f = unwords
        [ "cp", sb f ++ "/bin/rbtree-throughput", "bin/Main-" ++ n f ++ "-8" ]

unwords' = unwords . filter (/= "")

flags :: BuildOpts -> Flavor -> String
flags opts (Flavor t f h c) = unwords' ["-f" ++ cpp
                                       , extralibs
                                       ] 
  where
    cpp = concat $ catMaybes
      [ Just $ map toLower (opts^.benchmark)
      , mwhen t "tstruct"
      ]

    extralibs
      | opts^.benchmark == "hamt" = if t
                                      then "stm-containers-TStruct/"
                                      else "stm-containers-0.2.9/"
      | otherwise                 = ""

ghc :: Flavor -> String
ghc f
    |      f^.fine  && not (f^.hybrid) = g "mutable-fields"
    | not (f^.fine) && not (f^.hybrid) = g "mutable-fields-coarse"
    |      f^.fine  &&      f^.hybrid  = g "mutable-fields-hybrid"
    | not (f^.fine) &&      f^.hybrid  = g "mutable-fields-coarse-hybrid"
  where
    g s = "/localdisk/ryates/ghc-8/build-" ++ s ++ "/bin/ghc"

---------------------------------------------------------------------------------
-- Clean

clean :: CleanOpts -> IO ()
clean opts = do
    rm ".build-*"
    rm "dist"
    rm "../throughput/dist"
    rm "../random/pcg-random/dist"
    rm "stm-containers-TStruct/dist"
    rm "stm-containers-0.2.9/dist"
  where
    rm s = act opts ("rm -rf" ++ s)

---------------------------------------------------------------------------------
-- Run


run :: RunOpts -> IO ()
run opts = do
    red <- initRedirect opts ""
    forM_ (opts^.flavors) $ \f -> do
       let exe = opts^.benchmark ++ "-" ++ showFlavor f
           n = opts^.benchmark ++ "-" ++ showFlavor f
           main = "./bin/Main-" ++ n ++ "-8"
       forM_ [1..72] $ \t -> do
         let htm = hasHtm f !* maybe (max t 10) id (opts^.htmRetry)
             hle = hasHtm f !* maybe (max t 10) id (opts^.hleRetry)
             retry = "--htm-retry=" ++ show htm ++ " --hle-retry=" ++ show hle
             bloom = ""
             cmd = unwords' 
                     [ main
                     , "-e 100000"
                     , "-t " ++ show t
                     , "-m " ++ show (opts^.readMix)
                     , "-s " ++ show (opts^.milliseconds)
                     , "+RTS"
                       , "--stm-stats"
                       , "-qa" ++ opts^.affinity
                       , "-N" ++ show t
                       , "-ki4k -kc64k -kb4k -A1m"
                       , retry
                       , bloom
                       , maybe "" (("--stm-accum="++) . show) (opts^.heapStatAccum)
                     ]
         actRedirect opts red $ "perf stat -e tx-start,tx-capacity,tx-conflict -- " ++ cmd
    closeRedirect opts red
  where
    True  !* b = b
    False !* _ = 0

---------------------------------------------------------------------------------
main = do
    prog <- getProgName
    let p = info (helper <*> opts)
               (fullDesc <> progDesc "Benchmark builder." <> header prog)
    opts <- execParser p

    -- print opts

    case opts^.cmd of
        Clean co -> clean co
        Build bo -> build bo
        Run   ro -> run   ro

    m <- takeMVar globalFiles
    forM_ (M.elems m) hClose
    putMVar globalFiles M.empty -- Reset for interactive.