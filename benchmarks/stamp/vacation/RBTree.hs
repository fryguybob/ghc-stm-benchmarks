module RBTree
    ( RBTree
    , mkRBTree
    
    , insert
    , delete
    , update
    , get
    , contains

    , testMain
    ) where

import Prelude hiding (lookup)

import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Control.Exception

import Data.List (sort,inits)
import Debug.Trace

data Node k v 
    = Node { key    :: k
           , value  :: v
           , parent :: TVar (Node k v)
           , left   :: TVar (Node k v)
           , right  :: TVar (Node k v)
           , color  :: TVar Color
           }
    | Nil
    deriving (Eq)

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
        LT -> join $ lookupNode s <$> readTVar tl
        GT -> join $ lookupNode s <$> readTVar tr

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
                                 xppp <- parentOf xpp
                                 return (xp,xpp,xppp)
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
    r <- readTVar (root tree)
    x' <- loop r x
    when (isNode x) $ do
      c <- readTVar (color x)
      when (c /= Black) $ writeTVar (color x) Black
  where
    loop r x 
      | x == r    = return x
      | otherwise = do
          c <- colorOf x
          if c /= Black
            then return x
            else body x >>= loop r

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
                rotateL tree x
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
replace :: (Eq k, Eq v) => k -> v -> Node k v -> STM (Node k v)
replace k v n@(Node _ _ l r p c) = do
    let n' = Node k v l r p c
    -- update the rest of the tree:
    readTVar l >>= setField parent n'
    readTVar r >>= setField parent n'
    b <- isLeftBranch n
    readTVar p >>= setField (if b then left else right) n'
    return n'

deleteNode :: (Eq k, Eq v) => RBTree k v -> Node k v -> STM (Node k v)
deleteNode s p = do
    l <- readTVar (left  p)
    r <- readTVar (right p)
    p' <- if isNode l && isNode r
            then do
              s <- successor p
              replace (key s) (value s) p
              return s
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
                when (p' == ppr) $ writeTVar (right pp) Nil
            writeTVar (parent p') Nil
    return p'

----------------------------------
-- Public API
--
mkRBTree :: STM (RBTree k v)
mkRBTree = RBTree <$> newTVar Nil

insert :: (Eq v, Ord k) => RBTree k v -> k -> v -> STM Bool
insert t k v = isNil <$> insert' t k v

delete :: (Eq v, Ord k) => RBTree k v -> k -> STM Bool
delete t k = do
    n <- lookup k t
    if isNode n
       then isNode <$> deleteNode t n
       else return False

update :: (Eq v, Ord k) => RBTree k v -> k -> v -> STM Bool
update t k v = do
    n <- insert' t k v
    case n of
      Node _ v' _ _ _ _ -> do
        when (v /= v') $ replace k v n >> return ()
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

inOrder :: Node k v -> STM [k]
inOrder Nil = return []
inOrder n   = do
  l <- readTVar (left n)
  ol <- inOrder l
  r <- readTVar (right n)
  or <- inOrder r
  return $ ol ++ [key n] ++ or

verifyOrder :: (Ord k) => Node k v -> STM ()
verifyOrder Nil = return ()
verifyOrder n   = do
  es <- inOrder n
  unless (sort es == es) $ error "Ordering."

verify :: (Eq k, Eq v, Ord k, Show v, Show k) => RBTree k v -> STM Int
verify t = do
  r <- readTVar (root t)
  if isNil r
    then return 1
    else do
      assertEq ("Root parent not Nil "  ++ show (key r)) (parent r) Nil
      assertEq ("Root color not Black " ++ show (key r)) (color  r) Black

      verifyOrder r

      verifyRedBlack r 0

assertM s v = do
  b <- v
  unless b $ error s

insertTest as = atomically $ do
  r <- mkRBTree
  forM_ as $ \a -> do
    assertM "Insert failed" $ insert r a a
    verify r

testMain = do
    forM_ (inits [4,6,9,1,8,7,2,3,0,5]) $ \as -> do
        putStrLn $ "Inserting " ++ show as
        insertTest as
