module Plotter.HpglToLines (hpglToLines) where

import Plotter.HpglCommand
import Data.List

data Pen = Up | Down deriving Show
type Plotter = (Pen, (Float, Float))

hpglToLines :: [HPGLCommand] -> [[(Float, Float)]]
hpglToLines hpgl = snd $ foldl' strip (initPlotter, []) hpgl
  where
    initPlotter = (Up, (0, 0))

strip :: (Plotter, [[(Float, Float)]]) -> HPGLCommand -> (Plotter, [[(Float, Float)]])
strip plotter                 (SC _)  = plotter
strip ((_,    p), lines)      PD      = ((Down, p),  ([p]:lines))
strip ((_,    p), lines)      PU      = ((Up,   p),  lines)
strip ((Up,   _), lines)      (MV p') = ((Up,   p'), lines)
strip ((Down, _), (ps:lines)) (MV p') = ((Down, p'), ((p':ps):lines))
