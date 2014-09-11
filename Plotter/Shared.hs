{-# LANGUAGE DeriveDataTypeable #-}

module Plotter.Shared ( pullPerStep
                      , spoolRadius
                      , degreesPerStep
                      , leftSpoolPoint
                      , rightSpoolPoint

                      , distance
                      , bounds

                      , MyPoint
                      , Bounds ) where

import Data.Typeable

type MyPoint = (Float, Float)
type Bounds = (Float, Float, Float, Float)

degreesPerStep = 2 :: Float
spoolRadius = 10 :: Float
leftSpoolPoint = (-250, 200)::MyPoint
rightSpoolPoint = (250, 200)::MyPoint
bounds = (-150, 150, -150, 150)::Bounds

pullPerStep = (degreesPerStep / 360) * spoolCircumference
  where spoolCircumference = 2 * pi * spoolRadius

distance :: MyPoint -> MyPoint -> Float
distance (x1, y1) (x2, y2) = sqrt (x'*x' + y'*y')
    where 
        x' = x1 - x2
        y' = y1 - y2
