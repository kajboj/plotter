{-# LANGUAGE DeriveDataTypeable #-}

module Plotter.Command ( Step(L, R, N)
                       , Command (PenDown, PenUp, Move) ) where
  
import Data.Typeable

data Step = L | R | N deriving (Eq, Show, Read)
data Command = PenDown | PenUp | Move (Step, Step) deriving (Show, Read, Typeable)
