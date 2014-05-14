
import TestData
import TestDataParser

import System
import System.Random

-- A sample implementation of a random test data generator 

-- Specifies how many threads to run concurrently
-- num_of_threads = 4

-- Specifies number of times each concurrency mode test is ran.
-- num_of_repeats = 5

-- Specifies the initial size of the list
-- init_list_size = 3000

-- Specifies number of operations each thread is to run.
-- num_of_ops = 3000

-- Specifies the ratio of operations to randomly generate.
-- (f,d,i) represents 
-- ratio of finds (f) : ratio of deletes (d) : ratio of inserts (i)
-- op_ratio = (4,2,3)

-- Specifies the range of random values to generate.
-- Note that this implicitly determines the type of
-- values the list should contain.
-- 
-- value_range :: (Show a,Read a) => (a,a)
-- value_range = ((0,4000)::(Int,Int))

-- The main interface. Generates a testcase file based on the
-- above configurations. Output the file with name specified by
-- the first input argument

main = do
  { args <- getArgs
  ; case args of
     [fname,threads,size,nops,finds,deletes,inserts,range] ->
       let num_of_threads = read threads :: Int
           init_list_size = read size :: Int
           num_of_ops = read nops :: Int
           op_ratio = (read finds, read deletes, read inserts)
           value_range = (0,read range :: Int)
           rsize = max (num_of_threads*num_of_ops) init_list_size
                  in do { ran_elems <- randomRIOs rsize value_range
                        ; let ran_list = take init_list_size ran_elems
                              ran_opargs = take (num_of_threads*num_of_ops) ran_elems
                        ; ran_ops_linear <- mapM (random_op op_ratio) ran_opargs
                        ; let ran_ops = distribute ran_ops_linear num_of_threads
                        ; let testdata = TestData { t_name      = fname   
                                                  , t_threads   = num_of_threads
                                                  , t_init_list = ran_list
                                                  , t_tasks     = ran_ops }
                        ; write_testdata testdata
                        ; return () }
     _         -> putStrLn "Where's the filename!?" }
 

random_op :: (Int,Int,Int) -> a -> IO (Op a)
random_op (f,d,i) val = do
  { x <- randomRIO (0,5000)
  ; let m = mod x (f+d+i)
  ; if m < f then return (Find val)
    else if (m >= f) && (m < (f+d)) 
         then return (Delete val)
         else return (Insert val) }

-- Auxiliary Functions

distribute :: [a] -> Int -> [[a]]
distribute no threads =
   let init = map (\ _ -> []) [1..threads]
       go :: [a] -> Int -> [[a]] -> [[a]]
       go [] _ acc = acc
       go (x:xs) cnt acc = 
          let idx = cnt `mod` threads
              acc' = take idx acc ++ [x : (acc !! idx)] ++ drop (idx+1) acc
          in go xs (cnt+1) acc'
   in go no 1 init   


              
-- Random Generators

-- Create n random values given n and the desired range
randomRIOs :: Random a => Int -> (a,a) -> IO [a]
randomRIOs 0 rs = return []
randomRIOs n rs = do { as <- randomRIOs (n-1) rs
                     ; a <- randomRIO rs
                     ; return (a:as) } 
