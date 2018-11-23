import System.Console.GetOpt
import System.Environment
import Data.Maybe (fromMaybe)

data BenchOpts = BenchOpts
    { _entries      :: Word
    , _threads      :: Int
    , _initOnly     :: Bool
    , _withoutTM    :: Bool
    , _mix          :: Double
    , _throughput   :: Int
    } deriving (Show)

benchDefs :: BenchOpts
benchDefs = BenchOpts
    { _entries    = 800
    , _threads    = 8
    , _initOnly   = False
    , _withoutTM  = False
    , _mix        = 90
    , _throughput = 1000
    }

benchOpts :: [OptDescr (BenchOpts -> BenchOpts)]
benchOpts =
  [ Option ['e'] ["entries"]
      (ReqArg (\v o -> o { _entries = read v}) "ENTRIES") "Number of values in the tree"
  , Option ['t'] ["threads"]
      (ReqArg (\v o -> o { _threads = read v}) "THREADS") "Number of threads"
  , Option ['i'] ["initOnly"]
      (NoArg (\o -> o { _initOnly = True }))         "Initialize only"
  , Option ['w'] ["withoutTM"]
      (NoArg (\o -> o { _withoutTM = True }))        "No transactions"
  , Option ['m'] ["mix"]
      (ReqArg (\v o -> o { _mix = read v}) "MIX")         "Read mix percent"
  , Option ['s'] ["throughput"]
      (ReqArg (\v o -> o { _throughput = read v}) "TIME")
      "Throughput runtime in milliseconds"
  ]

readOpt f v = read v

getBenchOpts = do
    prog <- getProgName
    let header = "Usage: " ++ prog
    argv <- getArgs
    case getOpt Permute benchOpts argv of
      (o, n, []) -> return (foldl (flip id) benchDefs o, n)
      (_, _, es) -> ioError (userError (concat es ++ usageInfo header benchOpts))

main = do
    as <- getBenchOpts

    print as
