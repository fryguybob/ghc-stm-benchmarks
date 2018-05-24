{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE BangPatterns              #-}
{- LANGUAGE Strict -}
-- #define TESTCODE
module TreapIORef
    ( Treap
    , mkTreap
    , insert
    , get
    , contains
    , delete

    , benchCode
#ifdef TESTCODE
    , verifyTreap
    , insertV
    , deleteV
#endif
    ) where

import qualified System.Random.PCG.Fast.Pure as R
import qualified Data.Vector.Unboxed.Mutable as U
import System.Random.PCG.Class (sysRandom)
import Data.Word (Word64, Word32, Word)
import Control.Monad
import Data.IORef

import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int
import GHC.Conc

benchCode :: String
benchCode = "TreapIORef"

------------------------------
-- Random 

-- | Constant for aligning RNG seed to cache-line
-- which usually is 64 Kb long (while seed only is 'Data.Word64').
cacheFactor :: Int
cacheFactor = 8

type RandomState = U.IOVector Word64

-- Obtains PCG state, generate random value and store new state
gen :: RandomState -> Int -> IO Word32
gen v !i = do
  let i' = i * cacheFactor
  st <- U.read v i'
  let (R.P st' x) = R.pair st
  U.write v i' st'
  return x


----------------------------------------------
-- mutable

type Mutable a = IORef a

readRef :: Mutable a -> IO a
readRef r = readIORef r

writeRef :: Mutable a -> a -> IO ()
writeRef r l = writeIORef r l

---------------------------------------------
-- Treap

type Key = Word
type Value = Word
type Priority = Word32 -- native to our PRNG

data Treap where
    Treap :: RandomState
          -> IORef Node
          -> Treap

data Node where
    Node :: !Key      -- Key
         -> !Value    -- Value
         -> !Priority -- Priority
         -> IORef Node -- Left
         -> IORef Node -- Right
         -> Node
    Nil :: Node

key    :: Node -> Key
key    (Node k _ _ _ _) = k

value  :: Node -> Value
value  (Node _ v _ _ _) = v

priority :: Node -> Priority
priority  (Node _ _ p _ _) = p


left   :: Node -> Mutable Node
left   (Node _ _ _ l _) = l

right  :: Node -> Mutable Node
right  (Node _ _ _ _ r) = r

instance Show Node where
  show Nil = "Nil"
  show (Node k v _ _ _) = show ("Node " ++ show k ++ " " ++ show v)

instance Eq Node where
  Nil == Nil = True
  Nil == _   = False
  _   == Nil = False

--  x   == y   = case reallyUnsafePtrEquality# x y of
--                 0# -> False
--                 _  -> True
  (Node k _ _ _ _) == (Node k' _ _ _ _) = k == k'

isNil :: Node -> Bool
isNil Nil = True
isNil _   = False

isNode = not . isNil

mkTreap :: IO Treap
mkTreap = do
  cn <- getNumCapabilities
  statev <- U.new (cn * cacheFactor)
  forM_ [0..cn-1] $ \i -> do
    seed <- sysRandom
    U.write statev (i * cacheFactor) seed
  Treap statev <$> newIORef Nil

-- Implementation somewhat follows: 
--  https://github.com/frol/completely-unscientific-benchmarks/blob/master/c%23/Program.cs
mkNode :: Treap -> Key -> Value -> IO Node
mkNode (Treap s _) k v = do
    cn <- do
        tid <- myThreadId
        fst `fmap` threadCapability tid
    p <- gen s cn 
    Node k v p <$> newIORef Nil <*> newIORef Nil

merge :: Node -> Node -> IO Node
merge Nil g   = return g
merge l   Nil = return l
merge l@(Node _ _ lp _ lr) g@(Node _ _ gp gl _)
  | lp < gp = do
    lrn <- readRef lr
    merge lrn g >>= writeRef lr
    return l
  | otherwise = do
    gln <- readRef gl
    merge l gln >>= writeRef gl
    return g

splitL :: Key -> Node -> IO (Node, Node)
splitL _   Nil = return (Nil, Nil)
splitL key n@(Node k _ _ l r)
  | k < key = do
    (f, s) <- readRef r >>= splitL key
    writeRef r f
    return (n, s)
  | otherwise = do
    (f, s) <- readRef l >>= splitL key
    writeRef l s
    return (f, n)

splitLEq :: Key -> Node -> IO (Node, Node)
splitLEq _   Nil = return (Nil, Nil)
splitLEq key n@(Node k _ _ l r)
  | k <= key = do
    (f, s) <- readRef r >>= splitLEq key
    writeRef r f
    return (n, s)
  | otherwise = do
    (f, s) <- readRef l >>= splitLEq key
    writeRef l s
    return (f, n)

merge3 :: Node -> Node -> Node -> IO Node
merge3 l e g = do
    l' <- merge l e
    merge l' g

split :: Key -> Node -> IO (Node, Node, Node)
split k n = do
    (lof, los) <- splitL   k n
    (egf, egs) <- splitLEq k los
    return (lof, egf, egs)

nodeContains :: Key -> Node -> IO Bool
nodeContains k n = do
    let loop Nil = return False
        loop (Node k' _ _ l r)
          | k < k'    = readRef l >>= loop
          | k > k'    = readRef r >>= loop
          | otherwise = return True
    loop n

nodeGet :: Key -> Node -> IO (Maybe Value)
nodeGet k n = do
    let loop Nil = return Nothing
        loop (Node k' v _ l r)
          | k < k'    = readRef l >>= loop
          | k > k'    = readRef r >>= loop
          | otherwise = return (Just v)
    loop n

get :: Treap -> Key -> IO (Maybe Value)
get (Treap _ n) k = readRef n >>= nodeGet k

contains :: Treap -> Key -> IO Bool
contains (Treap _ n) k = readRef n >>= nodeContains k

insert :: Treap -> Key -> Value -> IO Bool
insert t@(Treap s nr) k v = do
    n <- readRef nr
    b <- nodeContains k n
    if b
      then return False
      else do
        (l,e,g) <- split k n
        e' <- mkNode t k v
        merge3 l e' g >>= writeRef nr
        return True

delete :: Treap -> Key -> IO Bool
delete (Treap _ nr) k = do
    n <- readRef nr
    b <- nodeContains k n
    if not b
      then return False
      else do
        (l,e,g) <- split k n
        merge l g >>= writeRef nr
        return True

#ifdef TESTCODE
assertIO :: Show s => String -> s -> Bool -> IO ()
assertIO _ _ True  = return ()
assertIO s v False = error (s ++ "\n -- " ++ show v)

verifyNode :: Node -> IO Int
verifyNode Nil = return 0
verifyNode (Node k v p l r) = do
    ln <- readRef l
    rn <- readRef r

    when (isNode ln) $ do
        assertIO "left <"  (key ln, k)      (key ln < k)
        assertIO "prio >=" (priority ln, p) (priority ln >= p)
    when (isNode rn) $ do
        assertIO "right >" (key rn, k)      (key rn > k)
        assertIO "prio >=" (priority rn, p) (priority rn >= p)

    ls <- verifyNode ln
    rs <- verifyNode rn
    return (ls + rs + 1)

verifyTreap :: Treap -> IO Int
verifyTreap (Treap _ n) = readRef n >>= verifyNode

insertV :: Treap -> Key -> Value -> IO Bool
insertV t k v = do
    s1 <- verifyTreap t
    b1 <- contains t k
    b2 <- insert t k v
    b3 <- contains t k
    s2 <- verifyTreap t
    assertIO "insert check" (b1,b2,b3)
        (  (b1 && not b2 && b3)  -- already present, stays
        || (not b1 && b2 && b3)) -- not present, is added.
    mv <- get t k
    assertIO "insert value check" (mv, v, b2)
        (case mv of
            Nothing -> False
            Just v' -> v == v' || not b2)
    assertIO "insert size" (s1,s2,b2)
        (  (b2 && s1 == s2 - 1)
        || (not b2 && s1 == s2))
    return b2

deleteV :: Treap -> Key -> IO Bool
deleteV t k = do
    s1 <- verifyTreap t
    b1 <- contains t k
    b2 <- delete t k
    b3 <- contains t k
    s2 <- verifyTreap t
    assertIO "delete check" (b1,b2,b3)
        (  (not b1 && not b2 && not b3)  -- already gone, stays gone.
        || (b1 && b2 && not b3)) -- is present, goes away.
    assertIO "delete size" (s1,s2,b2)
        (  (b2 && s1 == s2 + 1)
        || (not b2 && s1 == s2))
    return b2
#endif
