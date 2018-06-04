module Names
    ( nameSet
    ) where

import Control.Lens
import Control.Applicative
import Data.List
import Data.List.Split

nameSet es = commonPrefix (map (trim '-' . trim '8'. name) es)
              & _1 %~ trim '-'

name = parseName
parseName x = case ns of
                (n:os) -> parseBench n os ^. non x
                []      -> x
  where
    ns = splitOn "-" . trimTo "Main-" $ x

    parseBench :: String -> [String] -> Maybe String
    parseBench n os = do
        b <- lookupPrefix n bs
        (v,f) <- lookupSuffix n vs <|> tstruct os
        return $ intercalate "-" $ filter (/= "") $ [b, f os, v]

    bs = [ ("rbtree",   "RBTree")
         , ("treap",    "Treap")
         , ("cuckoo",   "Cuckoo")
         , ("stmtrie",  "HAMT")
         , ("skiplist", "Skiplist")
         ]

    -- TODO: hle is determined by commandline parameters.
    tm ns
      | "hybrid" `elem` ns = "Hybrid"
      | otherwise           = "STM"
    io _ = ""

    tstruct ns
      | "TStruct" `elem` ns = Just ("TStruct", tm)
      | otherwise           = Nothing

    vs = [ ("mutstmref",  ("mut-ref",    tm))
         , ("mutstmcps",  ("mut-cps",    tm))
         , ("mutustm",    ("mut-unbox",  tm))
         , ("mutstm",     ("mut",        tm))
         , ("tvar",       ("TVar",       tm))
         , ("tstruct",    ("TStruct",    tm))
         , ("ioref",      ("IORef",      io))
         , ("mutsingle",  ("IO-mut",     io))
         , ("tvarcolor",  ("TVar-Color", tm))
         ]

lookupBy f x ((k,v):es)
    | k `f` x   = Just v
    | otherwise = lookupBy f x es
lookupBy _ _ [] = Nothing

lookupPrefix = lookupBy isPrefixOf
lookupSuffix = lookupBy isSuffixOf

sharedPrefix ::  Eq a => [[a]] -> [a]
sharedPrefix [] = []
sharedPrefix s = foldr1 sp2 s
  where
      sp2 l1 l2 = map fst . takeWhile (uncurry (==)) $ zip l1 l2

commonPrefix :: Eq a => [[a]] -> ([a], [[a]])
commonPrefix ss = (p, map (drop (length p)) ss)
  where
    p = sharedPrefix ss

trim :: Eq a => a -> [a] -> [a]
trim c = f . f
  where
    f = reverse . dropWhile (== c)

trimTo _ [] = []
trimTo p as@(_:bs)
  | p `isPrefixOf` as = drop (length p) as
  | otherwise         = trimTo p bs

-- Old, less organized approach here for reference
name' x = lookupContains
            x [ ("cuckoo-tstruct-int-fine", "Cuckoo-TStruct")
              , ("cuckoo-tstruct-fine", "Cuckoo-TStruct-k")
              , ("cuckoo-tvar-fine-simple", "Cuckoo-TVar-Simple")
              , ("cuckoo-tvar-fine", "Cuckoo-TVar")

              , ("treapioref",     "Treap-IORef")
              , ("treapmutsingle", "Treap-mut-IO")

              , ("treapmutstmref-TVar-coarse-hybrid", "Treap-HTM-mut-ref")
              , ("treapmutstmcps-TVar-coarse-hybrid", "Treap-HTM-mut-cps")
              , ("treapmutstm-TVar-coarse-hybrid",    "Treap-HTM-mut")
              , ("treaptvar-TVar-coarse-hybrid",      "Treap-HTM-TVar")
              , ("treaptstruct-TVar-coarse-hybrid",   "Treap-HTM-TStruct")

              , ("treapmutstmref", "Treap-STM-mut-ref")
              , ("treapmutstmcps", "Treap-STM-mut-cps")
              , ("treapmutstm",    "Treap-STM-mut")
              , ("treaptvar",      "Treap-STM-TVar")
              , ("treaptstruct",   "Treap-STM-TStruct")

              , ("rbtreeioref",     "RBTree-IORef")
              , ("rbtreemutsingle", "RBTree-mut-IO")
              , ("rbtreemutstm",    "RBTree-STM-mut")
              , ("rbtreetvarcolor", "RBTree-STM-TVar-Color")
              , ("rbtreetvar",      "RBTree-STM-TVar")
              , ("rbtreetstruct",   "RBTree-STM-TStruct")

              , ("IORef",         "Map")
              , ("HashMap",       "HashMap")
              , ("no-invariants", "RBTree-Fine")
              , ("coarse",        "STM-Coarse")
              , ("htm-bloom",     "Hybrid")
              , ("hle-bloom",     "HTM-Coarse")
              , ("fine-hle",      "HTM-Fine")

              , ("skiplist-tstruct-fine", "Skiplist-TStruct")
              , ("skiplist-tstruct", "Skiplist-TStruct-Hybrid")
              , ("skiplist",      "Skiplist-TVar")

              , ("stmtrie-tstruct-fine-old", "HAMT-TStruct-STM-old")
              , ("stmtrie-tstruct-fine-htm", "HAMT-TStruct-fine-HTM")
              , ("stmtrie-tstruct-fine", "HAMT-TStruct-STM")
              , ("stmtrie-fine-htm", "HAMT-TVar-Fine-HTM")
              , ("stmtrie-fine",  "HAMT-Fine")

              , ("tstruct-fine-htm",  "RBTree-TStruct-Fine-HTM")
              , ("tstruct-fine",  "RBTree-TStruct-STM")
              , ("tstruct",       "RBTree-TStruct-Hybrid")

              , ("fine-htm",      "RBTree-TVar-Fine-HTM")

              ] ^. non x

suffix x
   | length s > 0 = Just $ reverse s
   | otherwise    = Nothing
  where
    s = takeWhile digitOrDash . reverse $ x
    digitOrDash c = (c >= '0' && c <= '9') || c == '-'

lookupContains x ((k,v):es)
    | k `isInfixOf` x = Just (keepSuffix x v)
    | otherwise       = lookupContains x es
lookupContains _ [] = Nothing

keepSuffix x v =
    case suffix x of
      Just s  -> v ++ s
      Nothing -> v


