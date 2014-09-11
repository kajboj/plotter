{-# LANGUAGE DeriveDataTypeable #-}

module Plotter.Shared ( pullPerStep
                      , spoolRadius
                      , degreesPerStep
                      , leftSpoolPoint
                      , rightSpoolPoint

                      , distance
                      , bounds
                      , intersectCircles
                      , rotationSign
                      , apply2
                      , zipWith2
                      , pullSigns

                      , MyPoint
                      , Bounds ) where

import Data.Typeable
import Plotter.Command

type MyPoint = (Float, Float)
type Bounds = (Float, Float, Float, Float)

degreesPerStep = 2 :: Float
spoolRadius = 10 :: Float
leftSpoolPoint = (-250, 200)::MyPoint
rightSpoolPoint = (250, 200)::MyPoint
bounds = (-150, 150, -150, 150)::Bounds
pullSigns = (-1, 1)::(Float, Float)

pullPerStep = (degreesPerStep / 360) * spoolCircumference
  where spoolCircumference = 2 * pi * spoolRadius

distance :: MyPoint -> MyPoint -> Float
distance (x1, y1) (x2, y2) = sqrt (x'*x' + y'*y')
    where 
        x' = x1 - x2
        y' = y1 - y2

---- returns only bottom result as our strings are pulled by gravity
intersectCircles :: MyPoint -> Float -> MyPoint -> Float -> MyPoint
intersectCircles (x0, y0) r0 (x1, y1) r1 = (x3, y3)
  where
    x3 = x2 + h * (y1 - y0) / d
    y3 = y2 - h * (x1 - x0) / d
    d = distance (x0, y0) (x1, y1)
    a = (r0^2 - r1^2 + d^2) / (2*d)
    h = sqrt (r0^2 - a^2)
    x2 = x0 + a * (x1 - x0) / d
    y2 = y0 + a * (y1 - y0) / d

apply2 :: (a -> b) -> (a, a) -> (b, b)
apply2 f (x, y) = (f x, f y)

zipWith2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipWith2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

rotationSign :: Step -> Float
rotationSign L = -1
rotationSign R = 1
rotationSign N = 0