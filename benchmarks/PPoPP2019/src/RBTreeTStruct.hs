{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
module RBTreeTStruct
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

import GHC.Conc.Sync
import Control.Applicative
import Control.Monad
import Control.Exception

import Data.List (sort,inits)
import Data.Word
import Debug.Trace

import System.IO.Unsafe
import RBTreeNode

import GHC.Conc(unsafeIOToSTM)

benchCode :: String
benchCode = "RBTreeTStruct"

isNil :: Node -> Bool
isNil s = s == nil

isNode = not . isNil

newtype RBTree = RBTree { root :: TVar Node }

lookupNode :: Key -> Node -> STM Node
lookupNode k n
  | n == nil  = return nil
  | otherwise = do
    k' <- key n
    case compare k k' of
        EQ -> return n
        LT -> left n >>= lookupNode k
        GT -> right n >>= lookupNode k

lookup :: Word -> RBTree -> STM Node
lookup k t = readTVar (root t) >>= lookupNode' k

rotateLeft :: RBTree -> Node -> STM ()
rotateLeft s x = do
    r <- right x
    rl <- left r
    writeRight x rl
    when (isNode rl) $ writeParent rl x

    xp <- parent x
    writeParent r xp

    if isNil xp
      then writeTVar (root s) r
      else do
        xpl <- left xp
        if xpl == x
          then writeLeft xp r
          else writeRight xp r

    writeLeft   r x
    writeParent x r


rotateRight :: RBTree -> Node -> STM ()
rotateRight s x = do
    l <- left x
    lr <- right l
    writeLeft x lr
    when (isNode lr) $ writeParent lr x

    xp <- parent x
    writeParent l xp

    if isNil xp
      then writeTVar (root s) l
      else do
        xpr <- right xp
        if xpr == x
          then writeRight xp l
          else writeLeft  xp l

    writeRight  l x
    writeParent x l

setColor :: Color -> Node -> STM ()
setColor c n = writeColor n c

node f x
  | x == nil  = return nil
  | otherwise = f x

parentOf, leftOf, rightOf :: Node -> STM Node
parentOf = node parent
leftOf   = node left
rightOf  = node right

colorOf x 
  | x == nil  = return Black
  | otherwise = color x

isLeftBranch :: Node -> STM Bool
isLeftBranch x = do
  x' <- parentOf x >>= leftOf
  return $ x == x'

fixAfterInsertion :: RBTree -> Node -> STM ()
fixAfterInsertion s x 
  | x == nil  = return ()
  | otherwise = do
    setColor Red x

    loop x

    ro <- readTVar (root s)
    c <- color ro
    when (c /= Black) $ writeColor ro Black
  where
    loop x
      | x == nil = return ()
      | otherwise = do
        sr <- readTVar (root s)
        if sr == x
          then return ()
          else body x >>= loop

    body x = do
        xp <- parent x
        if isNil xp
          then return nil
          else do
            xpp <- parentOf xp
            xppl <- leftOf xpp
    
            c <- color xp
    
            if c /= Red
              then return nil
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
insert' :: RBTree -> Key -> Value -> STM Node
insert' s k v = do
    t <- readTVar (root s)
    if isNil t
      then do
        n <- mkNode k v Black
        writeTVar (root s) n
        return nil
      else loop t
  where
    loop t = key t >>= \k' -> case compare k k' of
               EQ -> return t
               LT -> handle t left  writeLeft
               GT -> handle t right writeRight

    handle t get set = do
        tc <- get t
        if isNode tc
         then loop tc
         else do
          n <- mkNode k v Black
          writeParentP n t -- non-transactional access
          set t n
          fixAfterInsertion s n
          return nil

successor :: Node -> STM Node
successor t
  | t == nil = return nil
  | otherwise = do
    r <- right t
    if isNode r
      then leftMost r
      else parent t >>= rightParent t
  where
    leftMost p = do
      l <- left p
      if isNil l 
        then return p
        else leftMost l

    rightParent c p 
      | p == nil  = return nil
      | otherwise = do -- Find the first parent further right
        r <- right p
        if r == c
          then parent p >>= rightParent p
          else return p

fixAfterDeletion :: RBTree -> Node -> STM ()
fixAfterDeletion tree x = do
    x' <- loop x
    when (isNode x') $ do
      c <- color x'
      when (c /= Black) $ writeColor x' Black
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

-- Update key and value in a node in place.
updateKV :: Key -> Value -> Node -> STM ()
updateKV k v n = do
    writeKey   n k
    writeValue n v

deleteNode :: RBTree -> Node -> STM Node
deleteNode s p = do
    l <- left  p
    r <- right p
    p' <- if isNode l && isNode r
            then do
              suc <- successor p
              k <- key suc
              v <- value suc
              updateKV k v p
              return suc
            else return p
    
    l' <- left p'
    rep <- if isNode l'
             then return l'
             else right p'
    pp <- parent p'
    if isNode rep
      then do
        writeParent rep pp
        if isNil pp
          then writeTVar (root s) rep
          else do
            ppl <- left pp
            if p' == ppl
              then writeLeft  pp rep
              else writeRight pp rep
        writeLeft   p' nil
        writeRight  p' nil
        writeParent p' nil
        c <- color p'
        when (c == Black) $ fixAfterDeletion s rep
      else if isNil pp
        then writeTVar (root s) nil
        else do
          c <- color p'
          when (c == Black) $ fixAfterDeletion s p'
          pp' <- parent p'
          when (isNode pp') $ do
            ppl <- left pp'
            if p' == ppl
              then writeLeft pp' nil
              else do
                ppr <- right pp'
                when (p' == ppr) $ writeRight pp' nil
            writeParent p' nil
    return p'

----------------------------------
-- Public API
--
mkRBTree :: STM (RBTree)
mkRBTree = RBTree <$> newTVar nil

insert :: RBTree -> Key -> Value -> STM Bool
insert t k v = do
    isNil <$> insert' t k v <* postVerify t

{-
verifyContains t k = do
    b <- contains t k
    if b
      then return ()
      else error ("Expected key " ++ show k)

verifyDelete t k = do
    b <- contains t k
    if not b
      then return ()
      else error ("Found deleted key " ++ show k)
-}
preVerify  _ = return ()
postVerify _ = return ()
-- preVerify  = verify'
-- postVerify = verify'

delete :: RBTree -> Key -> STM Bool
delete t k = do
    n <- lookup k t
    if isNode n
       then isNode <$> (preVerify t *> deleteNode t n <* postVerify t)
       else return False

update :: RBTree -> Key -> Value -> STM Bool
update t k v = do
    n <- insert' t k v
    if isNil n
      then return False
      else do
        v' <- value n
        when (v /= v') $ updateKV k v n
        return True

get :: RBTree -> Key -> STM (Maybe Value)
get t k = do
  n <- lookup k t
  if isNil n
    then return Nothing
    else Just <$> value n

contains :: RBTree -> Key -> STM Bool
contains t k = isNode <$> lookup k t

----------------------------------------------------
-- Test code
--
-- #define TESTCODE
#ifdef TESTCODE

unlessM bm a = do
  b <- bm
  unless b a

verifyRedBlack :: Node -> Int -> STM Int
verifyRedBlack n d
  | n == nil  = return 1
  | otherwise = do
    l <- left  n
    r <- right n
    c <- color n

    hl <- verifyRedBlack l (d + 1)
    hr <- verifyRedBlack r (d + 1)
    if hl == 0 || hr == 0
      then return 0
      else do
        when (hl /= hr) $ error ("Imbalance @depth=" ++ show d ++ " : " ++ show hl ++ " " ++ show hr)
        lineage l
        lineage r

        c <- color n
        if c == Red
          then do
            unlessM (isBlack l) $ key n >>= \k -> error ("Expected black left of "  ++ show k)
            unlessM (isBlack r) $ key n >>= \k -> error ("Expected black right of " ++ show k)
            return hl
          else return (hl + 1)
  where
    lineage c
      | c == nil = return ()
      | otherwise = do
          p <- parent c
          when (p /= n) $ error ("lineage")

    isBlack n
      | n == nil  = return True
      | otherwise = color n >>= \c -> return (c == Black)

assertEq s get v = get >>= \v' -> when (v /= v') $ error s

inOrder :: Node -> STM [Key]
inOrder n
  | n == nil  = return []
  | otherwise = do
    l <- left n
    ol <- inOrder l
    r <- right n
    or <- inOrder r
    k <- key n
    return $ ol ++ [k] ++ or

verifyOrder :: Node -> STM ()
verifyOrder n
  | n == nil  = return ()
  | otherwise = do
    es <- inOrder n
    unless (sort es == es) $ error "Ordering."

verifyLinks p n as
  | p == nil && n == nil = return ()
  | p == nil = do
    l <- left  n
    r <- right n
    
    assertM "left loop"  $ return (l `notElem` as)
    assertM "right loop" $ return (r `notElem` as)

    when (isNode l) $ verifyLinks n l (l:as)
    when (isNode r) $ verifyLinks n r (r:as)
  | n == nil = return ()
  | otherwise = do
    p' <- parent n
    assertM "parent mismatch" $ return (p == p')
    verifyLinks nil n as

verify' t = do
  r <- readTVar (root t)
  verifyLinks nil r []

debug :: String -> STM ()
debug s = return $ unsafePerformIO $ putStrLn s

verify :: RBTree -> STM Int
verify t = do
  debug $ "Verifying."
  r <- readTVar (root t)
  if isNil r
    then return 1
    else do
      k <- key r
      assertEq ("Root parent not Nil "  ++ show k) (parent r) nil
      assertEq ("Root color not Black " ++ show k) (color  r) Black

      verifyLinks nil r ([])
      verifyOrder r
      verifyRedBlack r 0

assertM s v = do
  b <- v
  unless b $ error s

insertTest :: [Key] -> IO Int
insertTest as = atomically $ do
  r <- mkRBTree
  debug $ "Tree made."
  forM_ as $ \a -> do
    debug $ "Inserting " ++ show a
    assertM "Insert failed" $ insert r a a
  verify r

deleteTest :: [Key] -> [Key] -> IO ()
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
        insertTest bs
#endif
