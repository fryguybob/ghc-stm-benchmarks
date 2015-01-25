{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens hiding (argument)

import Data.Bool
import Data.Char
import Data.List
import Data.Monoid

import Options.Applicative hiding (info)
import qualified Options.Applicative as O

import RunTracker.Database (createDatabase,addMachine,addRuns,addTest,addConfig)
import RunTracker.Git
import RunTracker.Parse
import RunTracker.Util

import System.Environment

data Mode = Create | Add | Query | Plot
    deriving (Show, Enum)

data Table = Test | Machine | Config | Run
    deriving (Show, Enum)


instance Read Table where
    readsPrec = readsPrecEnum Test

instance Read Mode where
    readsPrec = readsPrecEnum Create

readsPrecEnum z _ vs = tryParse [ (s, x)
                              | x <- [z ..]
                              , s <- let x' = show x in [x', map toLower x']
                              ]
  where
    tryParse [] = []
    tryParse ((s, r):ps) =
        case splitAt (length s) vs of
            (s',vs') | s == s'   -> [(r, vs')]
                     | otherwise -> tryParse ps

data RunTrackerOptions = RunTrackerOptions
    { _database :: Maybe FilePath
    , _file     :: Maybe FilePath
    , _query    :: Maybe String
    , _name     :: Maybe String
    , _table    :: Maybe Table
    }
makeLenses ''RunTrackerOptions

data GlobalOptions = GlobalOptions
    { _verbose  :: Bool
    , _dryRun   :: Bool
    }
makeLenses ''GlobalOptions

enumParser :: (Read e, Show e, Enum e) => e -> Parser e
enumParser z = argument auto (help $ "(" ++ modes ++ ")")
  where
    modes = concat . intersperse " | " . map show $ [z ..]

opts :: Parser RunTrackerOptions
opts = RunTrackerOptions
    <$> (optional . strOption) (long "database" <> short 'd' <> help "Database to use.")
    <*> (optional . strOption) (long "file"     <> short 'f' <> help "File to add.")
    <*> (optional . strOption) (long "query"    <> short 'q' <> help "Query string.")
    <*> (optional . strOption) (long "Name"     <> short 'n' <> help "Name.")
    <*> (optional (enumParser Test))

globalOpts :: Parser GlobalOptions
globalOpts = GlobalOptions
    <$> switch (long "verbose" <> short 'v' <> help "Show verbose output.")
    <*> switch (long "dry-run"              <> help "Dry run without performing any actions.")

main :: IO ()
main = do
    prog <- getProgName
    let p = O.info (helper <*> ((,,) <$> enumParser Create <*> opts <*> globalOpts))
                 ( fullDesc
                 <> progDesc "Benchmark run tracking."
                 <> header prog)
    (mode, opts, globals) <- execParser p
    setGlobalOptions globals
    mainRunTracker mode opts

setGlobalOptions :: GlobalOptions -> IO ()
setGlobalOptions opts = do
    setVerbosity . bool 0 3 $ opts^.verbose || opts^.dryRun
    setEffects $ opts^.dryRun.to not

with :: String -> (a -> Maybe b) -> a -> (b -> c) -> c
with s f (f -> Just x) act = act x
with s _ _ _ = error $ "No " ++ s ++ " given."

withDB :: RunTrackerOptions -> (FilePath -> IO ()) -> IO ()
withDB    = with "database" _database
withTable = with "table"    _table
withQuery = with "query"    _query
withName  = with "name"     _name
withFile  = with "file"     _file

mainRunTracker :: Mode -> RunTrackerOptions -> IO ()
mainRunTracker Create opts = withDB opts $ \d -> createDatabase d
mainRunTracker Add o = 
    withDB o $ \d ->withTable o $ \case
        Machine -> withName o $ \n ->                                            addMachine d n 
        Config  -> withName o $ \n -> withFile o $ \f -> getHashHead       f >>= addConfig  d n
        Test    -> withName o $ \n -> withFile o $ \f -> getHashHead       f >>= addTest    d n
        Run     ->                    withFile o $ \f -> parseRunsFromFile f >>= addRuns    d
mainRunTracker Query o =
    withQuery o $ \q -> withDB o $ \d -> do
        return ()
mainRunTracker Plot  o =
    withQuery o $ \q -> withDB o $ \d -> do
        return ()

