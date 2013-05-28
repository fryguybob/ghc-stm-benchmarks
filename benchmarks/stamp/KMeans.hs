{-# LANGUAGE ViewPatterns, RecordWildCards #-}
module KMeans
    ( Config(..)
    , cluster
    ) where

import qualified Random as MT

import Data.List
import qualified Data.Array as A
import qualified Data.Array.MArray as A
import Data.Array.IO
import Data.Word

import Control.Monad
import Control.Applicative
import Control.Monad.Random
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import System.Console.CmdArgs.Implicit

dist :: [Float] -> [Float] -> Float
dist as bs = sum . map (^2) . zipWith (-) as $ bs

nearest :: [Float] -> [[Float]] -> Maybe Int
nearest p ps = minimum' Nothing (3.40282347e+38)
                 [ (Just i, dist p p') 
                 | (i,p') <- zip [0..] ps
                 ]

minimum' :: a -> Float -> [(a,Float)] -> a
minimum' i _ []         = i
minimum' i m ((i',d):ds)
    | d/m < 0.99999 = minimum' i' d ds
    | otherwise     = minimum' i  m ds

extractMoments :: [Float] -> [Float]
extractMoments ds = m : [sum [(d - m)^i | d <- ds] / l | i <- [2..]]
  where
    l = fromIntegral $ length ds
    m = sum ds / l

zscoreTransform :: [[Float]] -> [[Float]]
zscoreTransform dss = [ [ (d - m0)/m1 | d <- ds ]
                      | ds <- dss
                      , let (m0:(sqrt->m1):_) = extractMoments ds
                      ]

data Config = Config
    { _threadsC    :: Int
    , _useZScore   :: Bool
    , _minClusters :: Int
    , _maxClusters :: Int
    , _thresholdC  :: Float
    }

cluster :: Config -> [[Float]] -> IO [[[Float]]]
cluster Config{..} as = do
    m <- A.newArray (0, l-1) (-1)
    sequence [runNormal (p n) ds m | n <- [_minClusters.._maxClusters]]
  where
    l = length ds
    ds | _useZScore = zscoreTransform as
       | otherwise  = as
    p = Parameters _threadsC _thresholdC

chunks = 3

sampleN :: Int -> [[Float]] -> IO [[Float]]
sampleN n as = do
    let (is,bs) = splitAt n as
    r <- A.newListArray (1,n) is :: IO (IOArray Int [Float])
    sequence_ [ do j <- randomRIO (1,i)
                   when (j <= n) $ A.writeArray r j b
              | (b,i) <- zip bs [1..]
              ] 
    A.getElems r

data Parameters = Parameters
    { _threads   :: Int
    , _threshold :: Float
    , _clusters  :: Int
    } 

runNormal :: Parameters -> [[Float]] -> IOUArray Int Int -> IO [[Float]]
runNormal Parameters{..} points membership = do
    let (iss,[t]) = splitAt _threads . take (_threads + 1) . zip [0,chunks..] . tailsN chunks $ points
    tasks <- newTVarIO t
    delta <- newTVarIO 0
    cs <- tvarArray _clusters (replicate (length . snd . head $ iss) $ 0)
    ls <- tvarArray _clusters 0
    loop tasks iss cs ls delta
  where
    tvarArray n v = A.listArray (0,n-1) <$> (replicateM n . newTVarIO $ v)

    mkClusters ts ls = do
        cs <- mapM readTVarIO (A.elems ts)
        vs <- mapM readTVarIO (A.elems ls)
        return [map (/fromIntegral l) c | (l,c) <- zip vs cs]

    tailsN _ [] = []
    tailsN n ts = ts : tailsN n (drop n ts)

    l = genericLength points

    loop tasks iss cs ls delta = do
        -- Reservoir sampling
        -- clusters <- sampleN _clusters points
--        clusters <- replicateM _clusters $ (points !!) <$> randomRIO (0,length points-1)
        gen <- MT.initRandom 7
        clusters <- replicateM _clusters $ (points !!) 
                                         . (`mod` length points) 
                                         . fromIntegral
                                        <$> MT.getRandom gen
        loop' clusters 0
      where
        loop' clusters n = do
          ts <- sequence [async (work clusters i is) | (i,is) <- iss]
          mapM_ wait ts
          
          clusters' <- mkClusters cs ls

          d <- readTVarIO delta
          if fromIntegral d / l > _threshold && n < 500
            then loop' clusters' (n+1)
            else return clusters'

        
        work clusters i is = do
            d <- workLoop i 0 is
            atomically $ modifyTVar' delta (+d)
          where
            workLoop s d [] = return d
            workLoop s d fs = do
                let (fs',ns) = splitAt chunks fs
                ds <- sequence 
                           [ do atomically (modifyTVar' v (zipWith (+) f) >> modifyTVar' l (+1))
                                m <- A.readArray membership i
                                if m /= c
                                  then A.writeArray membership i c >> return 1
                                  else return 0
                           | (i,f) <- zip [s..] fs'
                           , let (Just c) = nearest f clusters
                           , let v = cs A.! c
                           , let l = ls A.! c
                           ]
                (i,next) <- atomically $ do 
                      ts <- readTVar tasks
                      writeTVar tasks (s+chunks,ns)
                      return ts
                workLoop i (d + sum ds) next

main = do
    putStrLn "Hello"
