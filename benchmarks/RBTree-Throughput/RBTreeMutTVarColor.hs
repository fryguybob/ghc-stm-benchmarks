{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE MutableFields             #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE UnboxedTuples             #-}
module RBTreeMutTVarColor
    ( RBTree
    , mkRBTree

    , insert
    , delete
    , update
    , get
    , contains
#ifdef TESTCODE
    , testMain

    , verify
#endif
    ) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad
import Control.Exception

import Data.List (sort,inits)
import Debug.Trace

import GHC.Types
import GHC.Prim
import GHC.IO
import GHC.Int
import GHC.Conc

-- data Node k v where 
--    Node :: { key    :: !k
--            , value  :: !v
--            , parent :: mutable (Node k v)
--            , left   :: mutable (Node k v)
--            , right  :: mutable (Node k v)
--            , color  :: mutable Color
--            } :: STM (Node k v)
--    Nil :: Node k v
-- TODO: record syntax.

type TRef a = Ref# RealWorld a

readTRef :: TRef a -> STM a
readTRef r = STM (\s# -> readTRef# r s#)

writeTRef :: TRef a -> a -> STM ()
writeTRef r l = STM (\s1# -> case writeTRef# r l s1# of
                             s2# -> (# s2#, () #))

data Node k v where
    Node :: !k
         -> !v
         -> mutable (Node k v)
         -> mutable (Node k v)
         -> mutable (Node k v)
         -> TVar Color
         -> STM (Node k v)
    Nil :: Node k v

-- TODO: unbox Color.

key    :: Node k v -> k
key    (Node k _ _ _ _ _) = k

value  :: Node k v -> v
value  (Node _ v _ _ _ _) = v

parent :: Node k v -> TRef (Node k v)
parent (Node _ _ p _ _ _) = p

left   :: Node k v -> TRef (Node k v)
left   (Node _ _ _ l _ _) = l

right  :: Node k v -> TRef (Node k v)
right  (Node _ _ _ _ r _) = r

color  :: Node k v -> TVar Color
color  (Node _ _ _ _ _ c) = c

instance (Show k, Show v) => Show (Node k v) where
  show Nil = "Nil"
  show (Node k v _ _ _ _) = show ("Node " ++ show k ++ " " ++ show v)

instance Eq k => Eq (Node k v) where
  Nil == Nil = True
  Nil == _   = False
  _   == Nil = False

--  x   == y   = case reallyUnsafePtrEquality# x y of
--                 0# -> False
--                 _  -> True
-- TODO: This caused a problem in unarize where it found a mutable
-- UnaryRep in the alt.
  (Node k _ _ _ _ _) == (Node k' _ _ _ _ _) = k == k'

isNil :: Node k v -> Bool
isNil Nil = True
isNil _   = False

isNode = not . isNil

data Color = Red | Black
    deriving (Eq, Show, Read)

-- data RBTree k v where
--     { root :: mutable (Node k v) } :: STM (RBTree k v)

data RBTree k v where
    RBTree :: mutable (Node k v) -> STM (RBTree k v)

root :: RBTree k v -> TRef (Node k v)
root (RBTree n) = n

lookupNode :: Ord k => k -> Node k v -> STM (Node k v)
lookupNode _ Nil = return Nil
lookupNode s n@(Node k v _ tl tr _)
    = case compare s k of
        EQ -> return n
        LT -> readTRef tl >>= lookupNode s
        GT -> readTRef tr >>= lookupNode s

lookup :: Ord k => k -> RBTree k v -> STM (Node k v)
lookup k t = readTRef (root t) >>= lookupNode k

rotateLeft :: (Eq v, Eq k) => RBTree k v -> Node k v -> STM ()
rotateLeft s x = do
    r <- readTRef (right x)
    rl <- readTRef (left r)
    writeTRef (right x) rl
    when (isNode rl) $ writeTRef (parent rl) x

    xp <- readTRef (parent x)
    writeTRef (parent r) xp

    if isNil xp
      then writeTRef (root s) r
      else do
        xpl <- readTRef (left xp)
        if xpl == x
          then writeTRef (left  xp) r
          else writeTRef (right xp) r

    writeTRef (left   r) x
    writeTRef (parent x) r


rotateRight :: (Eq v, Eq k) => RBTree k v -> Node k v -> STM ()
rotateRight s x = do
    l <- readTRef (left x)
    lr <- readTRef (right l)
    writeTRef (left x) lr
    when (isNode lr) $ writeTRef (parent lr) x

    xp <- readTRef (parent x)
    writeTRef (parent l) xp

    if isNil xp
      then writeTRef (root s) l
      else do
        xpr <- readTRef (right xp)
        if xpr == x
          then writeTRef (right xp) l
          else writeTRef (left  xp) l

    writeTRef (right  l) x
    writeTRef (parent x) l

setField :: (Node k v -> TRef a) -> a -> Node k v -> STM ()
setField _ _ Nil = return ()
setField f v x   = writeTRef (f x) v

setColor :: Color -> Node k v -> STM ()
setColor _ Nil = return ()
setColor c n   = writeTVar (color n) c

-- TODO: This complains about matching kinds...
-- node _ Nil = return Nil
-- node f x   = f x
--
-- parentOf, leftOf, rightOf :: Node k v -> STM (Node k v)
-- parentOf = node (readTRef . parent)
-- leftOf   = node (readTRef . left)
-- rightOf  = node (readTRef . right)


readIfNode :: (Node k v -> TRef (Node k v)) -> Node k v -> STM (Node k v)
readIfNode _ Nil = return Nil
readIfNode f n   = readTRef (f n)

parentOf, leftOf, rightOf :: Node k v -> STM (Node k v)
parentOf = readIfNode parent
leftOf   = readIfNode left
rightOf  = readIfNode right

colorOf Nil = return Black
colorOf x   = readTVar (color x)

isLeftBranch :: (Eq k, Eq v) => Node k v -> STM Bool
isLeftBranch x = do
  x' <- parentOf x >>= leftOf
  return $ x == x'

fixAfterInsertion :: (Eq k, Eq v) => RBTree k v -> Node k v -> STM ()
fixAfterInsertion _ Nil = return ()
fixAfterInsertion s x = do
    setColor Red x

    loop x

    ro <- readTRef (root s)
    c <- readTVar (color ro)
    when (c /= Black) $ writeTVar (color ro) Black
  where
    loop Nil = return ()
    loop x = do
        sr <- readTRef (root s)
        if sr == x
          then return ()
          else body x >>= loop

    body x = do
        xp <- readTRef (parent x)
        if isNil xp
          then return Nil
          else do
            xpp <- parentOf xp
            xppl <- leftOf xpp
    
            c <- readTVar (color xp)
    
            if c /= Red
              then return Nil
              else do
                if xp == xppl
                  then handle x xp xpp rightOf rotateLeft  rotateRight
                  else handle x xp xpp leftOf  rotateRight rotateLeft

    handle x xp xpp f ra rb = do
        y <- f xpp
        c <- colorOf y
        if c == Red
          then do
            setColor Black xp
            setColor Black y
            setColor Red   xpp
            return xpp
          else do
            z <- f xp
            (x',xp',xpp') <- if x == z
                               then do
                                 ra s xp
                                 xp' <- parentOf xp
                                 xpp' <- parentOf xp'
                                 return (xp,xp',xpp')
                               else return (x,xp,xpp)
            setColor Black xp'
            setColor Red   xpp'
            when (isNode xpp') $ rb s xpp'
            return x'

-- Note: we differ here in not taking a node argument of an exiting
-- allocated node.  The behavior when that argument is NULL in the original
-- code is to have insert act like find.
insert' :: (Eq v, Ord k) => RBTree k v -> k -> v -> STM (Node k v)
insert' s k v = do
    t <- readTRef (root s)
    if isNil t
      then do
        b <- newTVar Black
        n <- Node k v Nil Nil Nil b
        writeTRef (root s) n
        return Nil
      else loop t
  where
    loop t = case compare k (key t) of
               EQ -> return t
               LT -> handle t left
               GT -> handle t right

    handle t f = do
        tc <- readTRef (f t)
        if isNode tc
         then loop tc
         else do
          b <- newTVar Black
          n <- Node k v t Nil Nil b
          writeTRef (f t) n
          fixAfterInsertion s n
          return Nil

successor :: (Eq k, Eq v) => Node k v -> STM (Node k v)
successor Nil = return Nil
successor t   = do
    r <- readTRef (right t)
    if isNode r
      then leftMost r
      else readTRef (parent t) >>= rightParent t
  where
    leftMost p = do
      l <- readTRef (left p)
      case l of
        Nil -> return p
        _   -> leftMost l

    rightParent _ Nil = return Nil
    rightParent c p   = do -- Find the first parent further right
      r <- readTRef (right p)
      if r == c
        then readTRef (parent p) >>= rightParent p
        else return p


fixAfterDeletion :: (Eq k, Eq v) => RBTree k v -> Node k v -> STM ()
fixAfterDeletion tree x = do
    x' <- loop x
    when (isNode x') $ do
      c <- readTVar (color x')
      when (c /= Black) $ writeTVar (color x') Black
  where
    loop x = do
      r <- readTRef (root tree)
      case () of
        () | x == r    -> return x
           | otherwise -> do
               c <- colorOf x
               if c /= Black
                 then return x
                 else body x >>= loop

    body x = do
      b <- isLeftBranch x
      if b
        then handle x rightOf leftOf  rotateRight rotateLeft
        else handle x leftOf  rightOf rotateLeft  rotateRight

    handle x fR fL rotateR rotateL = do
      s <- parentOf x >>= fR
      c <- colorOf s
      s' <- if c == Red
              then do
                setColor Black s
                parentOf x >>= setColor Red
                parentOf x >>= rotateL tree
                parentOf x >>= fR
              else return s
      cl <- fL s' >>= colorOf
      cr <- fR s' >>= colorOf 
      if cl == Black && cr == Black
        then setColor Red s' >> parentOf x
        else do
          s'' <- if cr == Black
                   then do
                     fL s' >>= setColor Black
                     setColor Red s'
                     rotateR tree s'
                     parentOf x >>= fR
                   else return s'
          p <- parentOf x
          colorOf p >>= (`setColor` s'')
          setColor Black p
          fR s'' >>= setColor Black
          rotateL tree p
          readTRef (root tree)

-- link in a new node replacing the given node with new
-- key and value.
replace :: (Eq k, Eq v) => k -> v -> RBTree k v -> Node k v -> STM (Node k v)
replace k v t n@(Node _ _ rp rl rr rc) = do
    p <- readTRef rp
    l <- readTRef rl
    r <- readTRef rr
    c <- readTVar rc
    c' <- newTVar c
    n' <- Node k v p l r c'
    -- update the rest of the tree:
    b <- isLeftBranch n
    setField parent n' l -- Set left's  parent to n'
    setField parent n' r -- set right's parent to n'
    if isNil p
      then writeTRef (root t) n'
      else setField (if b then left else right) n' p -- set parent's left or right to n'
    return n'

deleteNode :: (Eq k, Eq v) => RBTree k v -> Node k v -> STM (Node k v)
deleteNode s p = do
    l <- readTRef (left  p)
    r <- readTRef (right p)
    p' <- if isNode l && isNode r
            then do
              suc <- successor p
              replace (key suc) (value suc) s p
              return suc
            else return p
    
    l' <- readTRef (left p')
    rep <- if isNode l'
             then return l'
             else readTRef (right p')
    pp <- readTRef (parent p')
    if isNode rep
      then do
        writeTRef (parent rep) pp
        if isNil pp
          then writeTRef (root s) rep
          else do
            ppl <- readTRef (left pp)
            if p' == ppl
              then writeTRef (left  pp) rep
              else writeTRef (right pp) rep
        writeTRef (left   p') Nil
        writeTRef (right  p') Nil
        writeTRef (parent p') Nil
        c <- readTVar (color p')
        when (c == Black) $ fixAfterDeletion s rep
      else if isNil pp
        then writeTRef (root s) Nil
        else do
          c <- readTVar (color p')
          when (c == Black) $ fixAfterDeletion s p'
          pp' <- readTRef (parent p')
          when (isNode pp') $ do
            ppl <- readTRef (left pp')
            if p' == ppl
              then writeTRef (left pp') Nil
              else do
                ppr <- readTRef (right pp')
                when (p' == ppr) $ writeTRef (right pp') Nil
            writeTRef (parent p') Nil
    return p'

----------------------------------
-- Public API
--
mkRBTree :: STM (RBTree k v)
mkRBTree = RBTree Nil

-- insert :: (Eq v, Ord k) => RBTree k v -> k -> v -> STM Bool
insert t k v = isNil <$> insert' t k v <* postVerify t

preVerify  _ = return ()
postVerify _ = return ()
-- Comment out the type signatures for insert and delete for verify code.
-- preVerify  = verify'
-- postVerify = verify'

-- delete :: (Eq v, Ord k) => RBTree k v -> k -> STM Bool
delete t k = do
    n <- lookup k t
    if isNode n
       then isNode <$> (preVerify t *> deleteNode t n <* postVerify t)
       else return False

update :: (Eq v, Ord k) => RBTree k v -> k -> v -> STM Bool
update t k v = do
    n <- insert' t k v
    case n of
      Node _ v' _ _ _ _ -> do
        when (v /= v') $ replace k v t n >> return ()
        return True
      Nil -> return False


get :: Ord k => RBTree k v -> k -> STM (Maybe v)
get t k = do
  n <- lookup k t
  case n of
    Node _ v _ _ _ _ -> return (Just v)
    Nil              -> return Nothing

contains :: Ord k => RBTree k v -> k -> STM Bool
contains t k = isNode <$> lookup k t

----------------------------------------------------
-- Test code
--
#ifdef TESTCODE

unlessM bm a = do
  b <- bm
  unless b a

verifyRedBlack :: (Show k, Show v, Eq k, Eq v) => Node k v -> Int -> STM Int
verifyRedBlack Nil _ = return 1
verifyRedBlack n   d = do
    l <- readTRef (left  n)
    r <- readTRef (right n)
    c <- readTVar (color n)

    hl <- verifyRedBlack l (d + 1)
    hr <- verifyRedBlack r (d + 1)
    if hl == 0 || hr == 0
      then return 0
      else do
        when (hl /= hr) $ error ("Imbalance @depth=" ++ show d ++ " : " ++ show hl ++ " " ++ show hr)
        lineage l
        lineage r

        c <- readTVar (color n)
        if c == Red
          then do
            unlessM (isBlack l) $ error ("Expected black left of "  ++ show (key n))
            unlessM (isBlack r) $ error ("Expected black right of " ++ show (key n))
            return hl
          else return (hl + 1)
  where
    lineage Nil = return ()
    lineage c   = do
          p <- readTRef (parent c)
          when (p /= n) $ error ("lineage")

    isBlack Nil = return True
    isBlack n   = readTVar (color n) >>= \c -> return (c == Black)

assertEq s t v = readTRef t >>= \v' -> when (v /= v') $ error s

{-
inOrder :: Show k => Node k v -> STM [k]
inOrder Nil = trace "." $ return []
inOrder n   = do
  l <- readTRef (left n)
  ol <- trace "(" $ inOrder l

  c <- readTVar (color n)
  r <- trace (if c == Red then "r" else "b") $ readTRef (right n)
  -- r <- trace (show (key n)) $ readTRef (right n)
  or <- inOrder r
  trace ")" $ return $ ol ++ [key n] ++ or
-- -}
{--}
inOrder :: Show k => Node k v -> STM [k]
inOrder Nil = return []
inOrder n   = do
  l <- readTRef (left n)
  ol <- inOrder l
  r <- readTRef (right n)
  or <- inOrder r
  return $ ol ++ [key n] ++ or
-- -}
verifyOrder :: (Ord k, Show k) => Node k v -> STM ()
verifyOrder Nil = return ()
verifyOrder n   = do
  es <- inOrder n
  unless (sort es == es) $ error "Ordering."

verifyLinks Nil Nil _  = return ()
verifyLinks Nil n   as = do
    l <- readTRef (left n)
    r <- readTRef (right n)
    
--    case l of
--      Node k v _ _ _ _ -> print ("l: Node", k, v)
--      Nil              -> print  "l: Nil"

--    case r of
--      Node k v _ _ _ _ -> print ("r: Node", k, v)
--      Nil              -> print  "r: Nil"

    assertM "left loop"  $ return (l `notElem` as)
    assertM "right loop" $ return (r `notElem` as)

    when (isNode l) $ verifyLinks n l (l:as)
    when (isNode r) $ verifyLinks n r (r:as)
verifyLinks p   Nil _  = return ()
verifyLinks p   n   as = do
    p' <- readTRef (parent n)
    if p /= p'
      then print (p, p')
      else return ()
    assertM "parent mismatch" $ return (p == p')
    verifyLinks Nil n as

verify' t = do
  r <- readTRef (root t)
  verifyLinks Nil r []

verify :: (Eq k, Eq v, Ord k, Show v, Show k) => RBTree k v -> STM Int
verify t = do
  r <- readTRef (root t)
  if isNil r
    then print ("root is Nil") >> return 1
    else do

--      case r of
--        Node k v _ _ _ _ -> print ("Node", k, v)
--        Nil              -> print "Nil"

      assertEq ("Root parent not Nil "  ++ show (key r)) (parent r) Nil
      assertEq ("Root color not Black " ++ show (key r)) (color  r) Black

      verifyLinks Nil r ([])
      verifyOrder r
      verifyRedBlack r 0

assertM s v = do
  b <- v
  unless b $ error s

insertTest :: [Int] -> STM Int
insertTest as = do
  r <- mkRBTree
  forM_ as $ \a -> do
    -- print ("insert", a)
    assertM "Insert failed" $ insert r a a
    verify r
  verify r

deleteTest :: [Int] -> [Int] -> STM ()
deleteTest as bs = do
  r <- mkRBTree
  forM_ as $ \a -> do
    assertM "insert failed" $ insert r a a
    verify r
  forM_ bs $ \b -> do
    assertM "delete failed" $ delete r b
    verify r

testMain' = do
    insertTest [4,6,9,1,8,7,2]

testMain'' = do
    forM_ (inits [4,6,9,1,8,7,2,3,0,5]) $ \as -> do
        putStrLn $ "Inserting " ++ show as
        insertTest as

testMain''' = do
    forM_ (zip (inits [4,6,9,1,8,7,2,3,0,5]) (map reverse (inits [4,6,9,1,8,7,2,3,0,5]))) $ \(as,bs) -> do
        putStrLn $ "Inserting " ++ show as ++ " deleting " ++ show bs
        insertTest as

testMain = do
    let as = [92,86,34,84, 5,64, 1, 87,11,39
             ,17,15,13,66,63,38,69, 67,88,16
             , 9,95,31,96,19,33,21, 27,65,10
             ,23,32,80,41,36,14,37, 54,98,51
             ,55,45,43,97,61,60, 2, 12,49,85
             , 8,76,46,78,48,56,35,100,29,90
             ,99,70,73,52,81,20, 3, 68,22,83
             ,71,72, 4,74,47,94,77, 89,59,91
             ,93,75,25,50, 6,58, 7, 62,53,40
             ,26,24,42,30,18,28,79, 82,44,57
             ]
    forM_ (inits as) $ \bs -> do
        putStrLn $ "Inserting " ++ show bs
        insertTest bs
#endif
