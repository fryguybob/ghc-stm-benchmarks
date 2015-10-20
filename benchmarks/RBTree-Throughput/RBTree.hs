{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module RBTree
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

#ifdef PASTMTL2
import Control.TL2.STM
#else
import Control.Concurrent.STM
#endif
import Control.Applicative
import Control.Monad
import Control.Exception

import Data.List (sort,inits)
import Debug.Trace

data Node k v 
    = Node { key    :: !k
           , value  :: !v
           , parent :: TVar (Node k v)
           , left   :: TVar (Node k v)
           , right  :: TVar (Node k v)
           , color  :: TVar Color
           }
    | Nil

instance Eq k => Eq (Node k v) where
  Nil == Nil = True
  Nil == _   = False
  _   == Nil = False

  (Node k _ _ _ _ _) == (Node k' _ _ _ _ _) = k == k'

isNil :: Node k v -> Bool
isNil Nil = True
isNil _   = False

isNode = not . isNil

data Color = Red | Black
    deriving (Eq, Show, Read)

newtype RBTree k v = RBTree { root :: TVar (Node k v) }

lookupNode :: Ord k => k -> Node k v -> STM (Node k v)
lookupNode _ Nil = return Nil
lookupNode s n@(Node k v _ tl tr _)
    = case compare s k of
        EQ -> return n
        LT -> readTVar tl >>= lookupNode s
        GT -> readTVar tr >>= lookupNode s

lookup :: Ord k => k -> RBTree k v -> STM (Node k v)
lookup k t = readTVar (root t) >>= lookupNode k

rotateLeft :: (Eq v, Eq k) => RBTree k v -> Node k v -> STM ()
rotateLeft s x = do
    r <- readTVar (right x)
    rl <- readTVar (left r)
    writeTVar (right x) rl
    when (isNode rl) $ writeTVar (parent rl) x

    xp <- readTVar (parent x)
    writeTVar (parent r) xp

    if isNil xp
      then writeTVar (root s) r
      else do
        xpl <- readTVar (left xp)
        if xpl == x
          then writeTVar (left  xp) r
          else writeTVar (right xp) r

    writeTVar (left   r) x
    writeTVar (parent x) r


rotateRight :: (Eq v, Eq k) => RBTree k v -> Node k v -> STM ()
rotateRight s x = do
    l <- readTVar (left x)
    lr <- readTVar (right l)
    writeTVar (left x) lr
    when (isNode lr) $ writeTVar (parent lr) x

    xp <- readTVar (parent x)
    writeTVar (parent l) xp

    if isNil xp
      then writeTVar (root s) l
      else do
        xpr <- readTVar (right xp)
        if xpr == x
          then writeTVar (right xp) l
          else writeTVar (left  xp) l

    writeTVar (right  l) x
    writeTVar (parent x) l

setField :: (Node k v -> TVar a) -> a -> Node k v -> STM ()
setField _ _ Nil = return ()
setField f v x   = writeTVar (f x) v

setColor :: Color -> Node k v -> STM ()
setColor = setField color

node _ Nil = return Nil
node f x   = f x

parentOf, leftOf, rightOf :: Node k v -> STM (Node k v)
parentOf = node (readTVar . parent)
leftOf   = node (readTVar . left)
rightOf  = node (readTVar . right)

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

    ro <- readTVar (root s)
    c <- readTVar (color ro)
    when (c /= Black) $ writeTVar (color ro) Black
  where
    loop Nil = return ()
    loop x = do
        sr <- readTVar (root s)
        if sr == x
          then return ()
          else body x >>= loop

    body x = do
        xp <- readTVar (parent x)
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
    t <- readTVar (root s)
    if isNil t
      then do
        n <- Node k v
          <$> newTVar Nil
          <*> newTVar Nil
          <*> newTVar Nil
          <*> newTVar Black
        writeTVar (root s) n
        return Nil
      else loop t
  where
    loop t = case compare k (key t) of
               EQ -> return t
               LT -> handle t left
               GT -> handle t right

    handle t f = do
        tc <- readTVar (f t)
        if isNode tc
         then loop tc
         else do
          n <- Node k v
            <$> newTVar t
            <*> newTVar Nil
            <*> newTVar Nil
            <*> newTVar Black
          writeTVar (f t) n
          fixAfterInsertion s n
          return Nil

successor :: (Eq k, Eq v) => Node k v -> STM (Node k v)
successor Nil = return Nil
successor t   = do
    r <- readTVar (right t)
    if isNode r
      then leftMost r
      else readTVar (parent t) >>= rightParent t
  where
    leftMost p = do
      l <- readTVar (left p)
      case l of
        Nil -> return p
        _   -> leftMost l

    rightParent _ Nil = return Nil
    rightParent c p   = do -- Find the first parent further right
      r <- readTVar (right p)
      if r == c
        then readTVar (parent p) >>= rightParent p
        else return p


fixAfterDeletion :: (Eq k, Eq v) => RBTree k v -> Node k v -> STM ()
fixAfterDeletion tree x = do
    x' <- loop x
    when (isNode x') $ do
      c <- readTVar (color x')
      when (c /= Black) $ writeTVar (color x') Black
  where
    loop x = do
      r <- readTVar (root tree)
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
          readTVar (root tree)

-- link in a new node replacing the given node with new
-- key and value.
replace :: (Eq k, Eq v) => k -> v -> RBTree k v -> Node k v -> STM (Node k v)
replace k v t n@(Node _ _ p l r c) = do
    let n' = Node k v p l r c
    -- update the rest of the tree:
    b <- isLeftBranch n
    readTVar l >>= setField parent n'
    readTVar r >>= setField parent n'
    np <- readTVar p
    if isNil np
      then writeTVar (root t) n'
      else setField (if b then left else right) n' np
    return n'

deleteNode :: (Eq k, Eq v) => RBTree k v -> Node k v -> STM (Node k v)
deleteNode s p = do
    l <- readTVar (left  p)
    r <- readTVar (right p)
    p' <- if isNode l && isNode r
            then do
              suc <- successor p
              replace (key suc) (value suc) s p
              return suc
            else return p
    
    l' <- readTVar (left p')
    rep <- if isNode l'
             then return l'
             else readTVar (right p')
    pp <- readTVar (parent p')
    if isNode rep
      then do
        writeTVar (parent rep) pp
        if isNil pp
          then writeTVar (root s) rep
          else do
            ppl <- readTVar (left pp)
            if p' == ppl
              then writeTVar (left  pp) rep
              else writeTVar (right pp) rep
        writeTVar (left   p') Nil
        writeTVar (right  p') Nil
        writeTVar (parent p') Nil
        c <- readTVar (color p')
        when (c == Black) $ fixAfterDeletion s rep
      else if isNil pp
        then writeTVar (root s) Nil
        else do
          c <- readTVar (color p')
          when (c == Black) $ fixAfterDeletion s p'
          pp' <- readTVar (parent p')
          when (isNode pp') $ do
            ppl <- readTVar (left pp')
            if p' == ppl
              then writeTVar (left pp') Nil
              else do
                ppr <- readTVar (right pp')
                when (p' == ppr) $ writeTVar (right pp') Nil
            writeTVar (parent p') Nil
    return p'

----------------------------------
-- Public API
--
mkRBTree :: STM (RBTree k v)
mkRBTree = RBTree <$> newTVar Nil

insert :: (Eq v, Ord k) => RBTree k v -> k -> v -> STM Bool
insert t k v = isNil <$> insert' t k v <* postVerify t

preVerify  _ = return ()
postVerify _ = return ()
-- preVerify  = verify'
-- postVerify = verify'

delete :: (Eq v, Ord k) => RBTree k v -> k -> STM Bool
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
    l <- readTVar (left  n)
    r <- readTVar (right n)
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
          p <- readTVar (parent c)
          when (p /= n) $ error ("lineage")

    isBlack Nil = return True
    isBlack n   = readTVar (color n) >>= \c -> return (c == Black)

assertEq s t v = readTVar t >>= \v' -> when (v /= v') $ error s

{-
inOrder :: Show k => Node k v -> STM [k]
inOrder Nil = trace "." $ return []
inOrder n   = do
  l <- readTVar (left n)
  ol <- trace "(" $ inOrder l

  c <- readTVar (color n)
  r <- trace (if c == Red then "r" else "b") $ readTVar (right n)
  -- r <- trace (show (key n)) $ readTVar (right n)
  or <- inOrder r
  trace ")" $ return $ ol ++ [key n] ++ or
-- -}
{--}
inOrder :: Show k => Node k v -> STM [k]
inOrder Nil = return []
inOrder n   = do
  l <- readTVar (left n)
  ol <- inOrder l
  r <- readTVar (right n)
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
    l <- readTVar (left n)
    r <- readTVar (right n)
    
    assertM "left loop"  $ return (l `notElem` as)
    assertM "right loop" $ return (r `notElem` as)

    when (isNode l) $ verifyLinks n l (l:as)
    when (isNode r) $ verifyLinks n r (r:as)
verifyLinks p   Nil _  = return ()
verifyLinks p   n   as = do
    p' <- readTVar (parent n)
    assertM "parent mismatch" $ return (p == p')
    verifyLinks Nil n as

verify' t = do
  r <- readTVar (root t)
  verifyLinks Nil r []

verify :: (Eq k, Eq v, Ord k, Show v, Show k) => RBTree k v -> STM Int
verify t = do
  r <- readTVar (root t)
  if isNil r
    then return 1
    else do
      assertEq ("Root parent not Nil "  ++ show (key r)) (parent r) Nil
      assertEq ("Root color not Black " ++ show (key r)) (color  r) Black

      verifyLinks Nil r ([])
      verifyOrder r
      verifyRedBlack r 0

assertM s v = do
  b <- v
  unless b $ error s

insertTest :: [Int] -> IO Int
insertTest as = atomically $ do
  r <- mkRBTree
  forM_ as $ \a -> do
    assertM "Insert failed" $ insert r a a
  verify r

deleteTest :: [Int] -> [Int] -> IO ()
deleteTest as bs = atomically $ do
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
        insertTest as
#endif
