{-# LANGUAGE ViewPatterns #-}
import System.IO
import System.Environment
import qualified Data.Map as M
import Control.Monad (mapM_)
import Numeric
import Data.List (intercalate)
import Data.Char (toLower)

readHex' :: (Eq a, Num a) => String -> a
readHex' s = case s of 
    "(nil)" -> 0
    ('0':'x':s') -> go s'
    _            -> go s
  where go s = case readHex s of
              [(a, "")] -> a
              _         -> error $ "Failed to parse hex \"" ++ take 20 s ++ "\""

readBool :: String -> Bool
readBool "0" = False
readBool (map toLower -> "true" ) = True
readBool (map toLower -> "false") = False
readBool (map toLower -> "t"    ) = True
readBool (map toLower -> "f"    ) = False
readBool "1" = True

data TVar    = TVar    {                  tvExpected :: Int, tvNew :: Int } deriving (Show)
data TStruct = TStruct { tsOffset :: Int, tsExpected :: Int, tsNew :: Int } deriving (Show)
data TRec = TRec 
  { trCap    :: Int
  , trResult :: Bool
  , trHTM    :: Bool
  , trTime   :: Double
  , trTvs    :: [(Int,TVar)]
  , trTss    :: M.Map Int [TStruct]
  } deriving (Show)

class HasUpdate a where
  isUpdate :: a -> Bool

instance HasUpdate TVar where
  isUpdate (TVar e n) = e /= n

instance HasUpdate TStruct where
  isUpdate (TStruct _ e n) = e /= n

trec :: [[String]] -> (Maybe TRec, [[String]])
trec (["TREC:", t, r, h, s]:ss) = 
    let (tvs, ss')  = many tvar    ss  in
    let (tss, ss'') = many tstruct ss' in
        (Just $ TRec (readHex' t) (readBool r) (readBool h) (read s) tvs (multiMapFromList tss), ss'')
trec ss = (Nothing, ss)

tvar :: [[String]] -> (Maybe (Int, TVar), [[String]])
tvar (["TVAR:", t, e, n]:ss) = (Just $ (readHex' t, TVar (readHex' e) (readHex' n)), ss)
tvar ss = (Nothing, ss)

tstruct :: [[String]] -> (Maybe (Int, TStruct), [[String]])
tstruct (["TSTR:", t, o, e, n]:ss) = (Just $ (readHex' t, TStruct (readHex' o) (readHex' e) (readHex' n)), ss)
tstruct ss = (Nothing, ss)

many :: (a -> (Maybe b, a)) -> a -> ([b], a)
many f a = case go a of
              [] -> ([], a)
              rs -> (map fst rs, snd $ last rs)
  where
    go x  = case f x of
              (Nothing, x') -> []
              (Just b,  x') -> (b,x') : go x'

multiMapFromList :: Ord k => [(k, v)] -> M.Map k [v]
multiMapFromList ps = go ps M.empty
  where
    go [] m = m
    go ((k,v):ps) m = go ps (M.alter (f v) k m)

    f v Nothing   = Just [v]
    f v (Just vs) = Just (v:vs)

calcStats (TRec _ _ htm time tvs tss) = 
    [ show $ length tvs
    , show $ M.size tss
    , show $ ( (length $ filter (not . isUpdate) $ map snd tvs)
             + (length $ filter (not . isUpdate) $ concat $ M.elems tss))
    , show $ ( (length $ filter isUpdate $ map snd tvs)
             + (length $ filter isUpdate $ concat $ M.elems tss))
    , if htm then "1" else "0"
    , show time
    ]
printCSV vs = putStrLn $ intercalate ", " vs 

main = do
    [a] <- getArgs
    ss <- map words . lines <$> readFile a

    -- print (fst $ trec ss)

    let (ts, ss') = many trec ss

    let ts' = drop 50002 ts

    -- print $ map (map isUpdate . concat . M.elems . trTss) . take 10 $ ts'

    putStrLn "TVars, TStructs, Reads, Writes, HTMCommit, Time"
    mapM_ printCSV (map calcStats ts')
