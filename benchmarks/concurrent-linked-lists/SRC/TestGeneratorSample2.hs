
import TestData
import TestDataParser

import System
import System.Random

-- A sample implementation of a random test data generator 

-- Specifies how many threads to run concurrently
num_of_threads = 4

-- Specifies the concurrency modes to test in this test case
conc_modes = ["CAS","MLC","LAZY","CASusingSTM", "MLCusingSTM"]   -- takes too long,"STM"]
                 -- only use IO for single-threaded examples

-- Specifies number of times each concurrency mode test is ran.
num_of_repeats = 5

-- Specifies the initial size of the list
init_list_size = 3000

-- Specifies number of operations each thread is to run.
num_of_ops = 3000

-- Specifies the ratio of operations to randomly generate.
-- (f,d,i) represents 
-- ratio of finds (f) : ratio of deletes (d) : ratio of inserts (i)
op_ratio = (4,2,3)

-- Specifies the range of random values to generate.
-- Note that this implicitly determines the type of
-- values the list should contain.
-- 
-- value_range :: (Show a,Read a) => (a,a)
value_range = ((0,4000)::(Int,Int))


-- Options flags: 
--     -tX            Run tests with X number of threads
-- 
--     -c+X1..+Xn     Run tests with concurrency modes X1...Xn .You can choose a subset of these 
--                    concurrency modes, for example -c+CAS+IO, runs CAS and IO tests 
--                    sequentially. 
--    
--     -wX            Run tests with work load X. This means that each
--                    thread will run X numbers of operations.
--  
--     -o+X+Y+Z       Set ratio of operations. Where X:Y:Z such that
--                         - X is the ratio of find operations
--                         - Y is the ratio of delete operations
--                         - Z is the ratio of insert operations
--
--     -rX            Run each test cases repeated x number of times, return the
--                    median and average time of the x runs.
--
--     -sX            Run tests starting with an initial list of size X.
--
--
--     -fX            Specifies output file name. Results will be written to file X.
        
data TestOptions = TestOptions { o_threads   :: Int           --  -t
                               , o_modes     :: [String]      --  -c
                               , o_workload  :: Int           --  -w
                               , o_op_ratio  :: (Int,Int,Int) --  -o
                               , o_repeats   :: Int           --  -r
                               , o_init_size :: Int           --  -s
                               , o_filename  :: String }      --  -f

                               
instance Show TestOptions where
  show opts = "Number of threads: " ++ (show $ o_threads opts) ++ "\n" ++
              "Selected modes: " ++ (show $ o_modes opts) ++ "\n" ++
              "Work load per thread: " ++ (show $ o_workload opts) ++ "\n" ++
              "Operation ratio: " ++ (printOps $ o_op_ratio opts) ++ "\n" ++
              "Number of repeats per test: " ++ (show $ o_repeats opts) ++ "\n" ++
              "Initial size of list: " ++ (show $ o_init_size opts) ++ "\n" ++
              "Output file name: " ++ (o_filename opts) ++ "\n"
              where
                printOps (f,d,i) = (show f) ++ " finds : " ++ (show d) ++ " deletes : " ++ (show i) ++ " inserts"
                   
-- Default options. Set this to the most frequently used configurations
            
default_options :: TestOptions
default_options = TestOptions { o_threads   = num_of_threads
                              , o_modes     = conc_modes
                              , o_workload  = num_of_ops
                              , o_op_ratio  = op_ratio
                              , o_repeats   = num_of_repeats
                              , o_init_size = init_list_size
                              , o_filename  = "mytest" }

process_args :: [String] -> TestOptions
process_args args = 
  process_args' args default_options
  where
    process_args' (arg:args) opts =
      let (flag,fargs) = splitAt 2 arg 
      in case flag of
          "-t"  -> process_args' args (opts{ o_threads = read fargs })
          "-c"  -> let ms = partitionAt (/='+') fargs
                   in process_args' args (opts{ o_modes = ms })
          "-w"  -> process_args' args (opts{ o_workload = read fargs })
          "-o"  -> let [f,d,i] = partitionAt (/='+') fargs
                   in process_args' args (opts{ o_op_ratio = (read f,read d,read i) })
          "-r"  -> process_args' args (opts{ o_repeats = read fargs })
          "-s"  -> process_args' args (opts{ o_init_size = read fargs })
          "-f"  -> process_args' args (opts{ o_filename = fargs })
          trash -> error $ "Oi! No such flag: " ++ trash
    process_args' [] opts = opts

-- The main interface. Generates a testcase file based on the
-- above configurations. Output the file with name specified by
-- the first input argument
main = do
  { args <- getArgs
  ; let topts = process_args args
  ; let s_threads  = o_threads topts
        s_modes    = o_modes topts
        s_workload = o_workload topts
        s_op_ratio = o_op_ratio topts
        s_repeats  = o_repeats topts
        s_init_size = o_init_size topts
        s_fname     = o_filename topts
        rsize = max (s_threads*s_workload) s_init_size
  ; putStrLn $ (show topts) ++ "\n"
  ; ran_elems <- randomRIOs rsize value_range
  ; let ran_list = take s_init_size ran_elems
        ran_opargs = take (s_threads*s_workload) ran_elems
  ; ran_ops_linear <- mapM (random_op s_op_ratio) ran_opargs
  ; let ran_ops = distribute ran_ops_linear s_threads
  ; let testdata = TestData { t_name      = s_fname   
                            , t_threads   = s_threads
                            , t_modes     = s_modes
                            , t_repeats   = s_repeats
                            , t_init_list = ran_list
                            , t_tasks     = ran_ops }
  ; write_testdata testdata
  ; return () }
 

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

partitionAt :: Eq a => (a -> Bool) -> [a] -> [[a]]
partitionAt f as = 
  filter (/=[]) (partitionAt' f as)
  where
    partitionAt' _ [] = []
    partitionAt' f as = let (v,rest) = span f as
                        in v:(partitionAt' f (drop 1 rest))
              
-- Random Generators

-- Create n random values given n and the desired range
randomRIOs :: Random a => Int -> (a,a) -> IO [a]
randomRIOs 0 rs = return []
randomRIOs n rs = do { as <- randomRIOs (n-1) rs
                     ; a <- randomRIO rs
                     ; return (a:as) } 
