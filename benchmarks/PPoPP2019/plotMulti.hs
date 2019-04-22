{-# LANGUAGE BangPatterns #-}
import System.Environment
import Data.List
import Control.Monad
import qualified Data.Map as M
import Text.Read
import Data.Char (toLower)

-- benchdata: run-time 1.0047227869945345 no-kill-time 1.0047204369911924 transactions 1727020 prog RBTREE-MUT-hybrid threads 1 entries 100000 code RBTreeMutUSTM
readLog :: Bool -> FilePath -> IO [[String]]
readLog threads log = do
  ls <- map words . filter ("benchdata:" `isPrefixOf`) . lines <$> readFile log
  return $ (if threads then ["Threads", log] else [log])
         : [ if threads then [t, show v] else [show v]
         | [_,_,s,_,_,_,n,_,_,_,t,_,_,_,_] <- map (take 15) ls
         , let v = read' n / read' s
         ]

getMaxY :: [[String]] -> Double
getMaxY (_:rs) = maximum . map read . concat $ rs

getY0 :: [[String]] -> Double
getY0 (_:r1:rs) = maximum . map read' $ r1

average :: [Double] -> Double
average vs = let (t,n) = foldl' (\(!b,!c) a -> (a+b,c+1)) (0,0) vs in realToFrac t / realToFrac n

aggregate :: ([Double] -> Double) -> [[String]] -> [[String]]
aggregate op ((t:hs):rs) = (t : M.keys m) : (transpose . (ts:) .  map (map op' . transpose) . M.elems $ m)
  where
   op' = show . op . map read'
   hs' = map dropSuffix hs
   (ts:cs) = transpose rs
   m = mmFromList $ zip hs' cs

read' :: (Read a) => String -> a
read' s
  | Just v <- readMaybe s = v
  | otherwise = error $ "Could not read: " ++ s

dropSuffix :: String -> String
dropSuffix log = reverse . drop 1 . dropWhile (/= '-') . reverse $ log

mmFromList :: (Ord k) => [(k,v)] -> M.Map k [v]
mmFromList ps = go ps M.empty
  where
    go [] m = m
    go ((k,v):ps) m
      | Just vs <- k `M.lookup` m = go ps (M.insert k (v:vs) m)
      | otherwise = go ps (M.insert k [v] m)

getTicks :: [[String]] -> [Int]
getTicks (_:rs) = ticks
  where
    (ts:_) = transpose rs
    m = maximum $ map (floor . read') ts

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

ops :: [(String, [Double] -> Double)]
ops = [ ("ave", average)
      , ("max", maximum)
      , ("min", minimum)
      ]
lookupOp :: String -> [Double] -> Double
lookupOp k
  | Just op <- k' `lookup` ops = op
  | otherwise = error $ "Could not find op: " ++ k
  where
    k' = map toLower k

main :: IO ()
main = do
    (t:out:op:as) <- getArgs
    cs <- (aggregate (lookupOp op) . foldl1 colPlus) <$> zipWithM readLog (True:repeat False) as
    let d = formatColumns cs
        maxY  = "var maxY = " ++ show (getMaxY cs)
        y0    = "var y0   = " ++ show (getY0   cs)
        ticks = "var ticks = " ++ show (getTicks cs)
    
    applyTemplate "plot/plot.html.template" out
        (M.fromList [("title", t), ("data", d), ("maxY", maxY), ("y0", y0), ("ticks", ticks)])
