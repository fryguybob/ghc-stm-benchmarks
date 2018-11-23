import System.Environment
import Data.List

asT vs = "(" ++ intercalate "," vs ++ ")"

tcs nt nc ns = [ s
               | c <- map transpose ts
               , t <- c
               , s <- t
               ]
  where
    n = nt * nc * ns
    ts = chunksOf nc . chunksOf nt $ [0..n-1]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n vs = take n vs : chunksOf n (drop n vs)


main = do
    as <- getArgs
    case as of
        [a,b,c] | [((t,c,s), [])] <- reads (asT [a,b,c]) -> mapM_ print (tcs t c s)
        _ -> mapM_ putStrLn $
                [ "Unknown input: " ++ show as
                , "Usage: mktopo THREADS CORES SOCKETS"
                , "   Print thread assignment filling cores on each socket first, than"
                , "   hyperthreads on each core."
                , "   The arguments are threads per core, cores per socket, and total sockets."
                , "   This assumes a particular ordering that may not be correct for"
                , "   a particular OS and architecture."
                ]

-- The existing files are produced with the following arguments:
-- $GHC_COMPILERS/hybrid/bin/runhaskell mktopo.hs 2 4  1 > topo-cores-sockets-threads-8
-- $GHC_COMPILERS/hybrid/bin/runhaskell mktopo.hs 2 6  1 > topo-cores-sockets-threads-12
-- $GHC_COMPILERS/hybrid/bin/runhaskell mktopo.hs 2 10 1 > topo-cores-sockets-threads-20
-- $GHC_COMPILERS/hybrid/bin/runhaskell mktopo.hs 2 12 1 > topo-cores-sockets-threads-24
-- $GHC_COMPILERS/hybrid/bin/runhaskell mktopo.hs 2 16 1 > topo-cores-sockets-threads-32
-- $GHC_COMPILERS/hybrid/bin/runhaskell mktopo.hs 2 18 1 > topo-cores-sockets-threads-34
-- $GHC_COMPILERS/hybrid/bin/runhaskell mktopo.hs 2 10 2 > topo-cores-sockets-threads-40
-- $GHC_COMPILERS/hybrid/bin/runhaskell mktopo.hs 2 12 2 > topo-cores-sockets-threads-48
-- $GHC_COMPILERS/hybrid/bin/runhaskell mktopo.hs 2 16 2 > topo-cores-sockets-threads-64
-- $GHC_COMPILERS/hybrid/bin/runhaskell mktopo.hs 2 18 2 > topo-cores-sockets-threads-72

