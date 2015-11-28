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
                      , pullSigns

                      , hPair
                      , getHPair
                      , (<*>)
                      , (<$>)
                      , pure

                      , MyPoint
                      , Bounds ) where

import Data.Typeable
import Plotter.Command
import Control.Applicative (Applicative, pure, (<*>), (<$>))

degreesPerStep = (1/4076 * 360) :: Float
spoolRadius = 25 :: Float
leftSpoolPoint = (-425, 800)::MyPoint
rightSpoolPoint = (425, 800)::MyPoint
bounds = (-70, 70, -60, 60)::Bounds
pullSigns = (-1, 1)::(Float, Float)

type MyPoint = (Float, Float)
type Bounds = (Float, Float, Float, Float)

newtype HPair a = HPair (a, a) deriving Show

hPair :: (a, a) -> HPair a
hPair (x, y) = HPair (x, y) 

getHPair :: HPair a -> (a, a)
getHPair (HPair x) = x

instance Functor HPair where
  fmap f (HPair (x, y)) = HPair (f x, f y)

instance Applicative HPair where
  pure x = HPair (x, x)
  (<*>) (HPair (f, g)) (HPair (x, y)) = HPair (f x, g y)

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

rotationSign :: Step -> Float
rotationSign L = -1
rotationSign R = 1
rotationSign N = 0
