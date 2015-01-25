module RunTracker.Util
    ( whenM

    , debug
    , info
    , warn
    , effectfully
    , systemEffectfully

    , setVerbosity
    , setEffects
    ) where

import Control.Applicative
import Control.Monad

import Data.Bool
import Data.IORef

import System.IO.Unsafe
import System.Process
import System.Exit

performEffects :: IORef Bool
performEffects = unsafePerformIO $ newIORef True -- The irony.
{-# NOINLINE performEffects #-}

setEffects :: Bool -> IO ()
setEffects b = writeIORef performEffects b

getEffects :: IO Bool
getEffects = readIORef performEffects

effectfully :: IO () -> IO ()
effectfully act = whenM getEffects act

effectfully' :: Show a => String -> a -> b -> (a -> IO b) -> IO b
effectfully' s a z f = getEffects >>= bool (info (s ++ show a) >> return z) (f a)

systemEffectfully :: String -> IO ExitCode
systemEffectfully s = effectfully' "system: " s ExitSuccess system

verbosityLevel :: IORef Int
verbosityLevel = unsafePerformIO $ newIORef 0
{-# NOINLINE verbosityLevel #-}

setVerbosity :: Int -> IO ()
setVerbosity level = writeIORef verbosityLevel level

getVerbosity :: IO Int
getVerbosity = readIORef verbosityLevel

debug :: String -> IO ()
debug s = whenM ((>2) <$> getVerbosity) $ putStrLn s

info :: String -> IO ()
info s = whenM ((>1) <$> getVerbosity) $ putStrLn s

warn :: String -> IO ()
warn s = whenM ((>0) <$> getVerbosity) $ putStrLn s

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond act = do
    b <- cond
    when b act


