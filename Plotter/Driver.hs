module Plotter.Driver (hpglToCommands, HPGLCommand(PU, PD, MV, SC), actualEndPoint) where

import Plotter.Command
import Plotter.Shared
import Plotter.HpglCommand

type Line = ([Command], MyPoint)

hpglToCommands :: MyPoint -> Bounds -> [HPGLCommand] -> [Command]
hpglToCommands _ _ [] = []

hpglToCommands prev scale (PD:moves) = (PenDown:hpglToCommands prev scale moves)
hpglToCommands prev scale (PU:moves) = (PenUp:hpglToCommands prev scale moves)
hpglToCommands prev scale (SC newScale:moves) = hpglToCommands prev newScale moves
hpglToCommands prev scale (MV point:moves) = commands ++ hpglToCommands actualEndPoint scale moves
  where
    (commands, actualEndPoint) = lineSteps prev scaledPoint
    scaledPoint = scalePoint scale bounds point

scalePoint :: Bounds -> Bounds -> MyPoint -> MyPoint
scalePoint (sMinX, sMaxX, sMinY, sMaxY) (minX, maxX, minY, maxY) (x, y) =
  (scale sMinX sMaxX minX maxX x, scale sMinY sMaxY minY maxY y)
  where
    scale sMin sMax min max v = min + (max-min)/(sMax-sMin) * (v - sMin)

lineSteps :: MyPoint -> MyPoint -> Line
lineSteps start end = (map Move steps, actualEndPoint start steps)
  where
    steps = calculateSteps leftSpoolPoint rightSpoolPoint start end

actualEndPoint :: MyPoint -> [(Step, Step)] -> MyPoint
actualEndPoint start steps = intersectCircles leftSpoolPoint newLeftRadius
  rightSpoolPoint newRightRadius
  where
    (newLeftRadius, newRightRadius) = zipWith2 (+) delta startRadiuses
    startRadiuses = apply2 (distance start) (leftSpoolPoint, rightSpoolPoint)
    delta = zipWith2 (*) pullSigns $ apply2 ((*pullPerStep) . stepCount) (unzip steps)
    stepCount = foldl addRotations 0
    addRotations count step = count + rotationSign step

lengthChange :: MyPoint -> MyPoint -> MyPoint -> MyPoint -> (Float, Float)
lengthChange leftPoint rightPoint start target = (newLeft - oldLeft, newRight - oldRight)
  where
    oldLeft = distance leftPoint start
    oldRight = distance rightPoint start
    newLeft = distance leftPoint target
    newRight = distance rightPoint target 

calculateSteps :: MyPoint -> MyPoint -> MyPoint -> MyPoint -> [(Step, Step)]
calculateSteps leftPoint rightPoint start target =
  toSteps $ lengthChange leftPoint rightPoint start target

toInts :: (Float, Float) -> [(Int, Int)]
toInts (leftDelta, rightDelta) = if leftSteps > rightSteps
  then map (\i -> (i, r i)) [1..leftSteps]
  else map (\i -> (r i, i)) [1..rightSteps]
  where
    leftSteps = abs $ round $ leftDelta / pullPerStep
    rightSteps = abs $ round $ rightDelta / pullPerStep
    minimum = min leftSteps rightSteps
    maximum = max leftSteps rightSteps
    x = (fromIntegral maximum) / (fromIntegral minimum)
    r i = round (fromIntegral i / x)

intsToSteps :: (Step, Step) -> [(Int, Int)] -> [(Step, Step)]
intsToSteps (leftStep, rightStep) ints = zip (stepify leftStep left) (stepify rightStep right)
  where
    left = map fst ints
    right = map snd ints

stepify :: Step -> [Int] -> [Step]
stepify step [] = []
stepify step (x:xs) = if x == 0
  then (N:stepify' 0 xs)
  else stepify' (-1) (x:xs)
  where
    stepify' prev [] = []
    stepify' prev (x:xs) = (s:stepify' x xs)
      where
        s = if prev == x
          then N
          else step

toSteps :: (Float, Float) -> [(Step, Step)]
toSteps (leftDelta, rightDelta) = intsToSteps steps ints
  where
    steps = (leftRotation leftDelta, rightRotation rightDelta)
    ints = toInts (leftDelta, rightDelta)

leftRotation :: Float -> Step
leftRotation 0 = N
leftRotation lengthChange = if lengthChange > 0
  then L
  else R

rightRotation :: Float -> Step
rightRotation 0 = N
rightRotation lengthChange = if lengthChange > 0
  then R
  else L