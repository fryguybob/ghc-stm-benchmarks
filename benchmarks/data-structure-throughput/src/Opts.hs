module Opts
    ( Opts(..)
    , getOpts
    ) where

import System.Environment
import qualified Data.Map as M
import Data.Word

data Opts = Opts
    { _entries      :: !Word
    , _threads      :: !Int
    , _initOnly     :: !Bool
    , _withoutTM    :: !Bool
    , _nowrites     :: !Bool
    , _atomicGroups :: !Int
    , _mix          :: !Double
    , _throughput   :: !Int
    } deriving (Show)

type OptMap = M.Map Char String

-- To avoid the optparse-applicative dependency, a simple subset of
-- functionality. WARNING: missing fuctionality includes error checking!
opt :: Read v => v -> Char -> OptMap -> v
opt def c m =
    case c `M.lookup` m of
        Just "" -> def
        Just v  -> read v
        Nothing -> def

flag :: Char -> OptMap -> Bool 
flag c m =
    case c `M.lookup` m of
        Just "" -> True
        Nothing -> False

buildMap :: [String] -> OptMap
buildMap as = M.fromList (split as)
  where
    split [] = []
    split (('-':[c]):as) = let (bs,as') = go as []
                           in  (c, concat $ reverse bs) : split as'

    go [] bs = (bs,[])
    go as@(('-':[c]):_) bs = (bs,as)
    go (a:as) bs = go as (a:bs)

getOpts :: IO Opts
getOpts = do
    as <- getArgs
    let m = buildMap as
    return $ Opts
        (opt   800 'e' m)
        (opt     8 't' m)
        (flag      'i' m)
        (flag      'w' m)
        (flag      'n' m)
        (opt     1 'g' m)
        (opt    90 'm' m)
        (opt  1000 's' m)

{-
 - The optparse-applicative version would be something like this:

opts = Opts
    <$> (option auto)
        (value 800 <> long "entries"      <> short 'e' <> help "Number of values in the tree")
    <*> (option auto)
        (value 8   <> long "threads"      <> short 't' <> help "Number of threads")
    <*> switch
        (             long "initOnly"     <> short 'i' <> help "Initialize only")
    <*> switch
        (             long "withoutTM"    <> short 'w' <> help "No transactions")
    <*> switch
        (             long "nowrites"     <> short 'n' <> help "Noish writes.  Inserts all use same key x and deletes all use same key y across all threads. ")
    <*> (option auto)
        (value 1   <> long "atomicGroups" <> short 'g' <> help "Lookups per transaction")
    <*> (option auto)
        (value 90  <> long "mix"          <> short 'm' <> help "Read mix percent")
    <*> (option auto)
        (value 1000<> long "throughput"   <> short 's' <> help "Throughput runtime in milliseconds")


getOpts :: IO Opts
getOpts = do
    prog <- getProgName
    let p = info (helper <*> opts)
                (fullDesc <> progDesc "Transactional data structure benchmark." <> header prog)
    execParser p

-}
