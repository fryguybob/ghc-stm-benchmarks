module RunTracker.Git
    ( getHashHead
    ) where



import Control.Concurrent
import Control.DeepSeq (rnf)
import Control.Exception

import RunTracker.Util

import System.Directory
import System.Exit
import System.IO
import System.Process

getHashHead :: FilePath -> IO String
getHashHead path = do
    path' <- canonicalizePath path

    info $ "Git head from path '" ++ path' ++ "':"
    out <- readShell "git rev-parse --verify HEAD" path'

    case lines out of
      [hash] -> return hash
      _      -> error $ "Filed to parse hash from output:\n" ++ out

-- The following is tweaked from System.Process.  There is not a variant of
-- readProcess that works for me.
forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)


readShell :: String -> FilePath -> IO String
readShell cmd working =
    mask $ \restore -> do
        let proc = (shell cmd) 
                      { cwd = Just working
                      , std_out = CreatePipe
                      }

        (_, Just out, _, p) <- createProcess proc
    
        flip onException
            (do hClose out
                terminateProcess p
                waitForProcess p) $ restore $ do

            output <- hGetContents out
            waitOut <- forkWait $ evaluate $ rnf output

            waitOut
            hClose out

            e <- waitForProcess p

            case e of
              ExitSuccess -> return output
              _           -> error $ "process failed: " ++ show e 
