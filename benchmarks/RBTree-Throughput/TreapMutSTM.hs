{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE MutableFields             #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE Strict                    #-}
-- #define TESTCODE
module TreapMutSTM
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

import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int
import GHC.Conc

benchCode :: String
benchCode = "TreapMutSTM"

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

type Mutable a = Ref# RealWorld a

readRef :: Mutable a -> STM a
readRef r = STM (\s# -> readTRef# r s#)

writeRef :: Mutable a -> a -> STM ()
writeRef r l = STM (\s1# -> case writeTRef# r l s1# of
                             s2# -> (# s2#, () #))

---------------------------------------------
-- Treap

type Key = Word
type Value = Word
type Priority = Word32 -- native to our PRNG

data Treap where
    Treap :: RandomState
          -> mutable Node
          -> STM Treap

data Node where
    Node :: !Key      -- Key
         -> !Value    -- Value
         -> !Priority -- Priority
         -> mutable Node -- Left
         -> mutable Node -- Right
         -> STM Node
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

mkTreap :: STM Treap
mkTreap = do
  state <- unsafeIOToSTM $ do
      cn <- getNumCapabilities
      statev <- U.new (cn * cacheFactor)
      forM_ [0..cn-1] $ \i -> do
        seed <- sysRandom
        U.write statev (i * cacheFactor) seed
      return statev
  Treap state Nil -- This isn't ideal...

-- Implementation somewhat follows: 
--  https://github.com/frol/completely-unscientific-benchmarks/blob/master/c%23/Program.cs
mkNode :: Treap -> Key -> Value -> STM Node
mkNode (Treap s _) k v = do
    p <- unsafeIOToSTM $ do
        cn <- do
            tid <- myThreadId
            fst `fmap` threadCapability tid
        gen s cn 
    Node k v p Nil Nil

merge :: Node -> Node -> STM Node
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

splitL :: Key -> Node -> STM (Node, Node)
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

splitLEq :: Key -> Node -> STM (Node, Node)
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

merge3 :: Node -> Node -> Node -> STM Node
merge3 l e g = do
    l' <- merge l e
    merge l' g

split :: Key -> Node -> STM (Node, Node, Node)
split k n = do
    (lof, los) <- splitL   k n
    (egf, egs) <- splitLEq k los
    return (lof, egf, egs)

nodeContains :: Key -> Node -> STM Bool
nodeContains k n = do
    let loop Nil = return False
        loop (Node k' _ _ l r)
          | k < k'    = readRef l >>= loop
          | k > k'    = readRef r >>= loop
          | otherwise = return True
    loop n

nodeGet :: Key -> Node -> STM (Maybe Value)
nodeGet k n = do
    let loop Nil = return Nothing
        loop (Node k' v _ l r)
          | k < k'    = readRef l >>= loop
          | k > k'    = readRef r >>= loop
          | otherwise = return (Just v)
    loop n

get :: Treap -> Key -> STM (Maybe Value)
get (Treap _ n) k = readRef n >>= nodeGet k

contains :: Treap -> Key -> STM Bool
contains (Treap _ n) k = readRef n >>= nodeContains k

insert :: Treap -> Key -> Value -> STM Bool
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

delete :: Treap -> Key -> STM Bool
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
assertSTM :: Show s => String -> s -> Bool -> STM ()
assertSTM _ _ True  = return ()
assertSTM s v False = error (s ++ "\n -- " ++ show v)

verifyNode :: Node -> STM Int
verifyNode Nil = return 0
verifyNode (Node k v p l r) = do
    ln <- readRef l
    rn <- readRef r

    when (isNode ln) $ do
        assertSTM "left <"  (key ln, k)      (key ln < k)
        assertSTM "prio >=" (priority ln, p) (priority ln >= p)
    when (isNode rn) $ do
        assertSTM "right >" (key rn, k)      (key rn > k)
        assertSTM "prio >=" (priority rn, p) (priority rn >= p)

    ls <- verifyNode ln
    rs <- verifyNode rn
    return (ls + rs + 1)

verifyTreap :: Treap -> STM Int
verifyTreap (Treap _ n) = readRef n >>= verifyNode

insertV :: Treap -> Key -> Value -> STM Bool
insertV t k v = do
    s1 <- verifyTreap t
    b1 <- contains t k
    b2 <- insert t k v
    b3 <- contains t k
    s2 <- verifyTreap t
    assertSTM "insert check" (b1,b2,b3)
        (  (b1 && not b2 && b3)  -- already present, stays
        || (not b1 && b2 && b3)) -- not present, is added.
    mv <- get t k
    assertSTM "insert value check" (mv, v, b2)
        (case mv of
            Nothing -> False
            Just v' -> v == v' || not b2)
    assertSTM "insert size" (s1,s2,b2)
        (  (b2 && s1 == s2 - 1)
        || (not b2 && s1 == s2))
    return b2

deleteV :: Treap -> Key -> STM Bool
deleteV t k = do
    s1 <- verifyTreap t
    b1 <- contains t k
    b2 <- delete t k
    b3 <- contains t k
    s2 <- verifyTreap t
    assertSTM "delete check" (b1,b2,b3)
        (  (not b1 && not b2 && not b3)  -- already gone, stays gone.
        || (b1 && b2 && not b3)) -- is present, goes away.
    assertSTM "delete size" (s1,s2,b2)
        (  (b2 && s1 == s2 + 1)
        || (not b2 && s1 == s2))
    return b2
#endif
