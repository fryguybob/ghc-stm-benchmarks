import GHC.Conc
import SkipListNode

main = do
  x <- newNodeIO 10

  y <- atomically (readKey nil)
  a <- atomically (readValue nil)
  z <- atomically (getNode x 1 2)
--  y <- atomically (readKey x)

  print (y,a,z)
