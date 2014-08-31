{-# LANGUAGE DeriveDataTypeable #-}

module Plotter.Shared ( pullPerStep
                      , spoolRadius
                      , degreesPerStep
                      , leftSpoolPoint
                      , rightSpoolPoint

                      , distance

                      , Point ) where

import Graphics.Gloss.Data.Point
import Data.Typeable

degreesPerStep = 10 :: Float
spoolRadius = 10 :: Float
leftSpoolPoint = (-250::Float, 200::Float)
rightSpoolPoint = (250::Float, 200::Float)

pullPerStep = (degreesPerStep / 360) * spoolCircumference
  where spoolCircumference = 2 * pi * spoolRadius

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt (x'*x' + y'*y')
    where 
        x' = x1 - x2
        y' = y1 - y2
