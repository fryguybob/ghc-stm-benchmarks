import System.Environment
import Data.List
import Control.Monad
import qualified Data.Map as M

-- In line before "HTM stats:"
-- Starts,     column 0
-- HTM-commit, column 6
-- STM-commit, column 5
-- 
-- In line before "Heap stats:"
-- HTM-fallback, column 0
-- HLE-fallback, column 5

readLog :: Bool -> FilePath -> IO [[String]]
readLog threads log = do
  ss <- findStats . lines <$> readFile log
  let rs = map rates ss
  let hs = map (log++) ["-full", "-fall", "-lock"]
  return $ (if threads then "Threads":hs else hs)
         : [ if threads then show t:vs else vs
           | (t, R full fall lock) <- zip [1..] rs
           , let vs = map show [full, fall, lock]
           ]

data Stats = S
    { starts      :: Int
    , stmCommit   :: Int
    , htmCommit   :: Int
    , htmFallback :: Int
    , hleFallback :: Int
    } deriving (Show)

read' s = case reads (filter (/= ',') s) of
            [(v, [])] -> v
            _         -> error $ "Can't read: " ++ s

findStats :: [String] -> [Stats]
findStats ls = go ls empty
  where
    empty = S 0 0 0 0 0
    go [] _ = []
    go (l:"HTM stats:": ls) s =
        let ws = words l
        in  go ls s { starts    = read' (ws !! 0)
                    , stmCommit = read' (ws !! 5)
                    , htmCommit = read' (ws !! 6)
                    } 
    go (l:"Heap stats:":ls) s =
        let ws = words l
        in  s { htmFallback = read' (ws !! 0)
              , hleFallback = read' (ws !! 5)
              } : go ls empty
    go (_:ls) s = go ls s

data Rates =
    R { full :: Double
      , fall :: Double
      , lock :: Double
      } deriving (Show)

hwFullCommit s =
    let a = fromIntegral $ starts s
        b = fromIntegral $ htmFallback s
    in a - b

hwFallbackCommit s =
    let a = fromIntegral $ htmCommit s
        b = hwFullCommit s
    in a - b

rates s =
    let rate a b = if b == 0 then 0 else a / b
        aFull = hwFullCommit s
        bFull = fromIntegral $ starts s
        aFall = hwFallbackCommit s
        bFall = fromIntegral $ htmFallback s
        aLock = fromIntegral $ stmCommit s
        bLock = fromIntegral $ hleFallback s
    in R { full = rate aFull bFull
         , fall = rate aFall bFall
         , lock = rate aLock bLock
         }

getTicks :: [[String]] -> [Int]
getTicks (_:rs) = ticks
  where
    (ts:_) = transpose rs
    m = maximum $ map read ts

    ticks 
     | m <= 16   = [1..m]
     | otherwise = [1, m `div` 4, m `div` 2, 3*(m `div` 4), m] 

colPlus :: [[a]] -> [[a]] -> [[a]]
colPlus as bs = transpose (transpose as ++ transpose bs)

formatColumns :: [[String]] -> String
formatColumns rs = unlines rs'
  where
    cs = transpose rs
    ls = map (succ . maximum . map length) cs

    pad s l = go s (l - length s)
    
    go s 0 = s
    go s l = ' ' : go s (l - 1)

    rs' = map concat . transpose . zipWith (\l c -> map (flip pad l) c) ls $ cs

applyTemplate :: FilePath -> FilePath -> M.Map String String -> IO ()
applyTemplate input output m = do
    ls <- lines <$> readFile input
    let os = map apply ls 
    writeFile output (unlines os)
  where
    apply l
      | "$$$" `isPrefixOf` l
      , [_, n] <- words l
      , Just v <- n `M.lookup` m = v
      | otherwise = l

main :: IO ()
main = do
    (t:out:as) <- getArgs
    cs <- foldl1 colPlus <$> zipWithM readLog (True:repeat False) as
    let d = formatColumns cs
        ticks = "var ticks = " ++ show (getTicks cs)
    
    applyTemplate "plot/plot-stats.html.template" out
        (M.fromList [("title", t), ("data", d), ("ticks", ticks)])
