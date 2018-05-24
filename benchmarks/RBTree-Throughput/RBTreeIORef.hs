{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module RBTreeIORef
    ( RBTree
    , mkRBTree

    , insert
    , delete
    , update
    , get
    , contains

    , benchCode
#ifdef TESTCODE
    , testMain

    , verify
#endif
    ) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad
import Control.Exception

import Data.IORef
import Data.List (sort,inits)
import Debug.Trace

benchCode :: String
benchCode = "RBTreeIORef"

data Node k v 
    = Node { key    :: !k
           , value  :: !v
           , parent :: IORef (Node k v)
           , left   :: IORef (Node k v)
           , right  :: IORef (Node k v)
           , color  :: IORef Color
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

newtype RBTree k v = RBTree { root :: IORef (Node k v) }

lookupNode :: Ord k => k -> Node k v -> IO (Node k v)
lookupNode _ Nil = return Nil
lookupNode s n@(Node k v _ tl tr _)
    = case compare s k of
        EQ -> return n
        LT -> readIORef tl >>= lookupNode s
        GT -> readIORef tr >>= lookupNode s

lookup :: Ord k => k -> RBTree k v -> IO (Node k v)
lookup k t = readIORef (root t) >>= lookupNode k

rotateLeft :: (Eq v, Eq k) => RBTree k v -> Node k v -> IO ()
rotateLeft s x = do
    r <- readIORef (right x)
    rl <- readIORef (left r)
    writeIORef (right x) rl
    when (isNode rl) $ writeIORef (parent rl) x

    xp <- readIORef (parent x)
    writeIORef (parent r) xp

    if isNil xp
      then writeIORef (root s) r
      else do
        xpl <- readIORef (left xp)
        if xpl == x
          then writeIORef (left  xp) r
          else writeIORef (right xp) r

    writeIORef (left   r) x
    writeIORef (parent x) r


rotateRight :: (Eq v, Eq k) => RBTree k v -> Node k v -> IO ()
rotateRight s x = do
    l <- readIORef (left x)
    lr <- readIORef (right l)
    writeIORef (left x) lr
    when (isNode lr) $ writeIORef (parent lr) x

    xp <- readIORef (parent x)
    writeIORef (parent l) xp

    if isNil xp
      then writeIORef (root s) l
      else do
        xpr <- readIORef (right xp)
        if xpr == x
          then writeIORef (right xp) l
          else writeIORef (left  xp) l

    writeIORef (right  l) x
    writeIORef (parent x) l

setField :: (Node k v -> IORef a) -> a -> Node k v -> IO ()
setField _ _ Nil = return ()
setField f v x   = writeIORef (f x) v

setColor :: Color -> Node k v -> IO ()
setColor = setField color

node _ Nil = return Nil
node f x   = f x

parentOf, leftOf, rightOf :: Node k v -> IO (Node k v)
parentOf = node (readIORef . parent)
leftOf   = node (readIORef . left)
rightOf  = node (readIORef . right)

colorOf Nil = return Black
colorOf x   = readIORef (color x)

isLeftBranch :: (Eq k, Eq v) => Node k v -> IO Bool
isLeftBranch x = do
  x' <- parentOf x >>= leftOf
  return $ x == x'

fixAfterInsertion :: (Eq k, Eq v) => RBTree k v -> Node k v -> IO ()
fixAfterInsertion _ Nil = return ()
fixAfterInsertion s x = do
    setColor Red x

    loop x

    ro <- readIORef (root s)
    c <- readIORef (color ro)
    when (c /= Black) $ writeIORef (color ro) Black
  where
    loop Nil = return ()
    loop x = do
        sr <- readIORef (root s)
        if sr == x
          then return ()
          else body x >>= loop

    body x = do
        xp <- readIORef (parent x)
        if isNil xp
          then return Nil
          else do
            xpp <- parentOf xp
            xppl <- leftOf xpp
    
            c <- readIORef (color xp)
    
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
insert' :: (Eq v, Ord k) => RBTree k v -> k -> v -> IO (Node k v)
insert' s k v = do
    t <- readIORef (root s)
    if isNil t
      then do
        n <- Node k v
          <$> newIORef Nil
          <*> newIORef Nil
          <*> newIORef Nil
          <*> newIORef Black
        writeIORef (root s) n
        return Nil
      else loop t
  where
    loop t = case compare k (key t) of
               EQ -> return t
               LT -> handle t left
               GT -> handle t right

    handle t f = do
        tc <- readIORef (f t)
        if isNode tc
         then loop tc
         else do
          n <- Node k v
            <$> newIORef t
            <*> newIORef Nil
            <*> newIORef Nil
            <*> newIORef Black
          writeIORef (f t) n
          fixAfterInsertion s n
          return Nil

successor :: (Eq k, Eq v) => Node k v -> IO (Node k v)
successor Nil = return Nil
successor t   = do
    r <- readIORef (right t)
    if isNode r
      then leftMost r
      else readIORef (parent t) >>= rightParent t
  where
    leftMost p = do
      l <- readIORef (left p)
      case l of
        Nil -> return p
        _   -> leftMost l

    rightParent _ Nil = return Nil
    rightParent c p   = do -- Find the first parent further right
      r <- readIORef (right p)
      if r == c
        then readIORef (parent p) >>= rightParent p
        else return p


fixAfterDeletion :: (Eq k, Eq v) => RBTree k v -> Node k v -> IO ()
fixAfterDeletion tree x = do
    x' <- loop x
    when (isNode x') $ do
      c <- readIORef (color x')
      when (c /= Black) $ writeIORef (color x') Black
  where
    loop x = do
      r <- readIORef (root tree)
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
          readIORef (root tree)

-- link in a new node replacing the given node with new
-- key and value.
replace :: (Eq k, Eq v) => k -> v -> RBTree k v -> Node k v -> IO (Node k v)
replace k v t n@(Node _ _ p l r c) = do
    let n' = Node k v p l r c
    -- update the rest of the tree:
    b <- isLeftBranch n
    readIORef l >>= setField parent n'
    readIORef r >>= setField parent n'
    np <- readIORef p
    if isNil np
      then writeIORef (root t) n'
      else setField (if b then left else right) n' np
    return n'

deleteNode :: (Eq k, Eq v) => RBTree k v -> Node k v -> IO (Node k v)
deleteNode s p = do
    l <- readIORef (left  p)
    r <- readIORef (right p)
    p' <- if isNode l && isNode r
            then do
              suc <- successor p
              replace (key suc) (value suc) s p
              return suc
            else return p
    
    l' <- readIORef (left p')
    rep <- if isNode l'
             then return l'
             else readIORef (right p')
    pp <- readIORef (parent p')
    if isNode rep
      then do
        writeIORef (parent rep) pp
        if isNil pp
          then writeIORef (root s) rep
          else do
            ppl <- readIORef (left pp)
            if p' == ppl
              then writeIORef (left  pp) rep
              else writeIORef (right pp) rep
        writeIORef (left   p') Nil
        writeIORef (right  p') Nil
        writeIORef (parent p') Nil
        c <- readIORef (color p')
        when (c == Black) $ fixAfterDeletion s rep
      else if isNil pp
        then writeIORef (root s) Nil
        else do
          c <- readIORef (color p')
          when (c == Black) $ fixAfterDeletion s p'
          pp' <- readIORef (parent p')
          when (isNode pp') $ do
            ppl <- readIORef (left pp')
            if p' == ppl
              then writeIORef (left pp') Nil
              else do
                ppr <- readIORef (right pp')
                when (p' == ppr) $ writeIORef (right pp') Nil
            writeIORef (parent p') Nil
    return p'

----------------------------------
-- Public API
--
mkRBTree :: IO (RBTree k v)
mkRBTree = RBTree <$> newIORef Nil

insert :: (Eq v, Ord k) => RBTree k v -> k -> v -> IO Bool
insert t k v = isNil <$> insert' t k v <* postVerify t

preVerify  _ = return ()
postVerify _ = return ()
-- Comment out the type signatures for insert and delete for verify code.
-- preVerify  = verify'
-- postVerify = verify'

delete :: (Eq v, Ord k) => RBTree k v -> k -> IO Bool
delete t k = do
    n <- lookup k t
    if isNode n
       then isNode <$> (preVerify t *> deleteNode t n <* postVerify t)
       else return False

update :: (Eq v, Ord k) => RBTree k v -> k -> v -> IO Bool
update t k v = do
    n <- insert' t k v
    case n of
      Node _ v' _ _ _ _ -> do
        when (v /= v') $ replace k v t n >> return ()
        return True
      Nil -> return False


get :: Ord k => RBTree k v -> k -> IO (Maybe v)
get t k = do
  n <- lookup k t
  case n of
    Node _ v _ _ _ _ -> return (Just v)
    Nil              -> return Nothing

contains :: Ord k => RBTree k v -> k -> IO Bool
contains t k = isNode <$> lookup k t

----------------------------------------------------
-- Test code
--
#ifdef TESTCODE

unlessM bm a = do
  b <- bm
  unless b a

verifyRedBlack :: (Show k, Show v, Eq k, Eq v) => Node k v -> Int -> IO Int
verifyRedBlack Nil _ = return 1
verifyRedBlack n   d = do
    l <- readIORef (left  n)
    r <- readIORef (right n)
    c <- readIORef (color n)

    hl <- verifyRedBlack l (d + 1)
    hr <- verifyRedBlack r (d + 1)
    if hl == 0 || hr == 0
      then return 0
      else do
        when (hl /= hr) $ error ("Imbalance @depth=" ++ show d ++ " : " ++ show hl ++ " " ++ show hr)
        lineage l
        lineage r

        c <- readIORef (color n)
        if c == Red
          then do
            unlessM (isBlack l) $ error ("Expected black left of "  ++ show (key n))
            unlessM (isBlack r) $ error ("Expected black right of " ++ show (key n))
            return hl
          else return (hl + 1)
  where
    lineage Nil = return ()
    lineage c   = do
          p <- readIORef (parent c)
          when (p /= n) $ error ("lineage")

    isBlack Nil = return True
    isBlack n   = readIORef (color n) >>= \c -> return (c == Black)

assertEq s t v = readIORef t >>= \v' -> when (v /= v') $ error s

{-
inOrder :: Show k => Node k v -> IO [k]
inOrder Nil = trace "." $ return []
inOrder n   = do
  l <- readIORef (left n)
  ol <- trace "(" $ inOrder l

  c <- readIORef (color n)
  r <- trace (if c == Red then "r" else "b") $ readIORef (right n)
  -- r <- trace (show (key n)) $ readIORef (right n)
  or <- inOrder r
  trace ")" $ return $ ol ++ [key n] ++ or
-- -}
{--}
inOrder :: Show k => Node k v -> IO [k]
inOrder Nil = return []
inOrder n   = do
  l <- readIORef (left n)
  ol <- inOrder l
  r <- readIORef (right n)
  or <- inOrder r
  return $ ol ++ [key n] ++ or
-- -}
verifyOrder :: (Ord k, Show k) => Node k v -> IO ()
verifyOrder Nil = return ()
verifyOrder n   = do
  es <- inOrder n
  unless (sort es == es) $ error "Ordering."

verifyLinks Nil Nil _  = return ()
verifyLinks Nil n   as = do
    l <- readIORef (left n)
    r <- readIORef (right n)
    
    assertM "left loop"  $ return (l `notElem` as)
    assertM "right loop" $ return (r `notElem` as)

    when (isNode l) $ verifyLinks n l (l:as)
    when (isNode r) $ verifyLinks n r (r:as)
verifyLinks p   Nil _  = return ()
verifyLinks p   n   as = do
    p' <- readIORef (parent n)
    assertM "parent mismatch" $ return (p == p')
    verifyLinks Nil n as

verify' t = do
  r <- readIORef (root t)
  verifyLinks Nil r []

verify :: (Eq k, Eq v, Ord k, Show v, Show k) => RBTree k v -> IO Int
verify t = do
  r <- readIORef (root t)
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
insertTest as = do
  r <- mkRBTree
  forM_ as $ \a -> do
    assertM "Insert failed" $ insert r a a
  verify r

deleteTest :: [Int] -> [Int] -> IO ()
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
