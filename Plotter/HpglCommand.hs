module Plotter.HpglCommand (HPGLCommand(PD, PU, MV, SC)) where

data HPGLCommand = PD | PU | MV (Float, Float) |
  SC (Float, Float, Float, Float) deriving Show
