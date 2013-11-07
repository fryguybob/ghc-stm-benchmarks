{-# LANGUAGE TemplateHaskell #-}
module Performance.Perf.CmdLine where

import Control.Lens

import Options.Applicative

import System.Environment

import Data.Monoid

data Options = Options
    { _output :: FilePath
    , _inputs :: [FilePath]
    , _query  :: String
    }

makeLenses ''Options

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption (mconcat
        [ long "output"
        , short 'o'
        , help "Output file"
        ])
    <*> arguments1 Just (help "Input files")
    <*> strOption (mconcat
        [ long "query"
        , short 'q'
        , help "Query"
        ])

defaultOptions :: IO Options
defaultOptions = do
    prog <- getProgName
    let p = info (helper <*> optionsParser)
          $ mconcat [ fullDesc, progDesc "Perf plot.", header prog ]
    execParser p

