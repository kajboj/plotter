{-# LANGUAGE DeriveDataTypeable #-}

module Plotter.Shared ( pullPerStep
                      , spoolRadius
                      , degreesPerStep
                      , leftSpoolPoint
                      , rightSpoolPoint

                      , distance

                      , MyPoint ) where

import Data.Typeable

type MyPoint = (Float, Float)

degreesPerStep = 1 :: Float
spoolRadius = 10 :: Float
leftSpoolPoint = (-250::Float, 200::Float)
rightSpoolPoint = (250::Float, 200::Float)

pullPerStep = (degreesPerStep / 360) * spoolCircumference
  where spoolCircumference = 2 * pi * spoolRadius

distance :: MyPoint -> MyPoint -> Float
distance (x1, y1) (x2, y2) = sqrt (x'*x' + y'*y')
    where 
        x' = x1 - x2
        y' = y1 - y2
