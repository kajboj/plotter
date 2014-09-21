module Plotter.Driver (hpglToCommands, HPGLCommand(PU, PD, MV, SC), actualEndPoint) where

import Plotter.Command
import Plotter.Shared
import Plotter.HpglCommand

type Line = ([Command], MyPoint)
type DistanceCalc = MyPoint -> Float

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
    --steps = calculateSteps leftSpoolPoint rightSpoolPoint start end
    steps = toSteps1 (distance end) (distanceToLine start end) start

distanceToLine :: MyPoint -> MyPoint -> MyPoint -> Float
distanceToLine start end point = sqrt (a*a - d*d)
  where
    d = (a*a - b*b + c*c) / (2*c)
    a = distance start point
    b = distance end point
    c = distance start end

actualEndPoint :: MyPoint -> [(Step, Step)] -> MyPoint
actualEndPoint start steps = intersectCircles leftSpoolPoint newLeftRadius
  rightSpoolPoint newRightRadius
  where
    (newLeftRadius, newRightRadius) = getHPair $ (+) <$> delta <*> startRadiuses
    startRadiuses = distance start <$> hPair (leftSpoolPoint, rightSpoolPoint)
    delta = (*) <$> (hPair pullSigns) <*> pulls
    pulls = (*pullPerStep) . stepCount <$> (hPair $ unzip steps)
    stepCount = foldl addRotations 0
    addRotations count step = count + rotationSign step

toSteps1 :: DistanceCalc -> DistanceCalc -> MyPoint -> [(Step, Step)]
toSteps1 distToTarget distToLine current = if closeEnough
  then []
  else (next : toSteps1 distToTarget distToLine newPoint)
  where
    next = nextStep distToTarget distToLine 100 current
    newPoint = nextPoint current next
    closeEnough = (distToTarget current) <= (distToTarget newPoint)

allSteps :: [(Step, Step)]
allSteps = filter (/= (N, N)) [(x, y) | x <- [L, N, R], y <- [L, N, R]]

nextStep :: DistanceCalc -> DistanceCalc -> Float -> MyPoint -> (Step, Step)
nextStep distToTarget distToLine prevDistToTarget current = minBy d goodSteps
  where
    d = distToLine . nextPoint current
    goodSteps = filter closerToTarget allSteps
    closerToTarget steps = (distToTarget $ nextPoint current steps) < prevDistToTarget

nextPoint :: MyPoint -> (Step, Step) -> MyPoint
nextPoint point steps = actualEndPoint point [steps]

minBy :: Ord b => (a -> b) -> [a] -> a
minBy f = foldl1 (\x y -> if (f x) < (f y) then x else y)

lengthChange :: MyPoint -> MyPoint -> MyPoint -> MyPoint -> (Float, Float)
lengthChange leftPoint rightPoint start target = (lengthDelta leftPoint, lengthDelta rightPoint)
  where
    lengthDelta point = (distance point target) - (distance point start)

calculateSteps :: MyPoint -> MyPoint -> MyPoint -> MyPoint -> [(Step, Step)]
calculateSteps leftPoint rightPoint start target =
  toSteps $ lengthChange leftPoint rightPoint start target

toSteps :: (Float, Float) -> [(Step, Step)]
toSteps (leftDelta, rightDelta) = intsToSteps steps ints
  where
    steps = (leftRotation leftDelta, rightRotation rightDelta)
    ints = toInts (leftDelta, rightDelta)


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