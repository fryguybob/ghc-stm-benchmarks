{-# LANGUAGE TemplateHaskell #-}
module Performance.Perf.Plot where

import Control.Lens

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend.Diagrams
import Diagrams.Prelude hiding ((&))
import Diagrams.Backend.Postscript
import Diagrams.Backend.Postscript.CmdLine
import Data.Default.Class

import Control.Monad
import Control.Applicative
import qualified Data.Map as M

import System.Environment

plotQuery :: String -> [(String,[Double])] -> FilePath -> IO ()
plotQuery n ds' output = do
    let ds = ds' & mapped._2 %~ zip [1..] :: [(String,[(Double,Double)])]
    e <- defaultEnv vectorAlignmentFns 600 600
    let (p,_) = runBackendR e $ toRenderable $ layout
        layout = def
           & layout_title .~ n
           & layout_y_axis. laxis_override .~ axisGridHide
           & layout_x_axis . laxis_override .~ axisGridHide
           & layout_plots .~ map (uncurry mkPlot) ds
           & layout_grid_last .~ False
 
    withArgs ["-o", output, "-w", "600"] $ defaultMain p
  where
    mkPlot n ps = def & plot_lines_title .~ n
                      & plot_lines_values .~ [ps]
                      & toPlot
