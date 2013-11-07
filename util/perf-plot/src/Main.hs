{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens

import Performance.Perf.Plot
import Performance.Perf.Parse
import Performance.Perf.CmdLine

import qualified Data.Map as M

getData :: String -> Perf -> Double
getData "time" (Perf n m t) = t
getData "commit" (Perf n m t)
    = case ("cpu/tx-commit/" `M.lookup` m) of
        (Just s) -> fromIntegral s
        _        -> error ("missing data." ++ show m)
getData "start" (Perf n m t)
    = case ("cpu/tx-start/" `M.lookup` m) of
        (Just s) -> fromIntegral s
        _        -> error ("missing data." ++ show m)
getData "rate" (Perf n m t)
    = case ("cpu/tx-commit/" `M.lookup` m, "cpu/tx-start/" `M.lookup` m) of
        (Just c, Just s) -> fromIntegral c / fromIntegral s
        _                -> error ("missing data." ++ show m)
getData l _ = error ("Unknown query " ++ l)

main :: IO ()
main = do
    opts <- defaultOptions
    rs <- parseInputs (opts^.inputs)
    case rs of
      Left  e  -> putStrLn e
      Right ds' -> do
        let ds = ds' & mapped.mapped %~ getData (opts^.query)
                     & zip (opts^.inputs)
        plotQuery (opts^.query) ds (opts^.output)
