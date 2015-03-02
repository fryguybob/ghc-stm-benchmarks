#!/bin/sh runhaskell
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Lens

import Data.Default.Class
import Data.List
import Data.List.Split
import Data.Maybe

import GHC.RTS.Events

import Numeric

import System.Directory
import System.FilePath
import System.Environment
import System.Process
import System.IO

import qualified Text.PrettyPrint.Boxes as B

data Stats a = Stats
    { _ave      :: Double
    , _minValue :: Double
    , _maxValue :: Double
    , _var      :: Double
    , _samples  :: Int
    , _statKey  :: a
    } deriving (Show,Eq)

makeLenses ''Stats

type Stats' = Stats ()

instance Default Stats' where
    def = Stats 0 0 0 0 0 ()


flatten :: Event -> [Event]
flatten (Event _ (EventBlock _ _ es)) = concatMap flatten es
flatten e = [e]

data Trans t = Begin t | End t
    deriving (Show, Eq)

toTrans (Event t (UserMessage "beginT")) = Just (Begin t) 
toTrans (Event t (UserMessage "endT"  )) = Just (End   t)
toTrans _ = Nothing

data Transaction t = T t t
    deriving (Show, Eq)

transactions :: [Trans t] -> [Transaction t]
transactions ((Begin b):(End e):ts) = T b e : transactions ts
transactions (_:ts) = transactions ts
transactions [] = []

lengthT :: Num t => Transaction t -> t
lengthT (T b e) = e - b

main = do
    [f] <- getArgs

    ts <- processFile f

    putStrLn $ "Transactions: " ++ (withCommas . show . length $ ts)

processFile f = do
    (Right el) <- readEventLogFromFile f
    return . transactions . mapMaybe toTrans . concatMap flatten . events . dat $ el

data Cell = TInt Int
          | TDeg Int Double
          | TCom Int Double
          deriving (Show, Eq)

printTable hs cs = B.printBox b
  where
    b = B.hsep 2 B.top . map (B.vcat B.right) . zipWith (:) (map B.text hs) $ cs'

    cs' = map (map boxup) cs

    boxup (TCom n v) = B.text . withCommas . flip (showFFloat (Just n)) [] $ v
    boxup (TDeg n v) = B.text . flip (showFFloat (Just n)) [] $ v
    boxup (TInt   v) = B.text . show $ v

withCommas s = v' ++ f
  where
    (v,f) = break (=='.') s
    v' = reverse . intercalate "," . chunksOf 3 . reverse $ v


analyse ts name = do
    putStrLn "Transaction time stats"
    printTable ["Average", "Min", "Max", "Std Dev", "Samples"]
      (map (flip map ss)
         [ (^.ave.to (TCom 2))
         , (^.minValue.to (TCom 2))
         , (^.maxValue.to (TCom 2))
         , (^.var.to sqrt.to (TCom 2))
         , (^.samples.to fromIntegral.to (TCom 0))
         ])
    writeFile "times.csv" . unlines . map (show . lengthT) $ ts
    callCommand ("./density.R " ++ "times.csv")
    callCommand ("cp density.pdf '" ++ name ++ ".pdf'")
  where
    ss = [stat (map (fromIntegral . lengthT) ts)] :: [Stats']



stat xs = def & ave      .~ u
              & minValue .~ minimum xs
              & maxValue .~ maximum xs
              & var      .~ sum [(x - u)**2 | x <- xs] / n
              & samples  .~ l
  where
    l = length xs
    n = fromIntegral $ l
    u = sum xs / n
