module Plotter.Driver (hpglToCommands, HPGLCommand(PD, PU, MV)) where

import Plotter.Command
import Plotter.Shared

data HPGLCommand = PD | PU | MV (Float, Float)

hpglToCommands :: MyPoint -> [HPGLCommand] -> [Command]
hpglToCommands prev [] = []
hpglToCommands prev (hpglCommand:coms) = (commands ++ hpglToCommands newPrev coms)
  where
    commands = case hpglCommand of
      PD -> [PenDown]
      PU -> [PenUp]
      MV point -> lineSteps prev point
    newPrev = case hpglCommand of
      MV point -> point
      _ -> prev

lineSteps :: MyPoint -> MyPoint -> [Command]
lineSteps start end = map Move steps
  where
    steps = calculateSteps leftSpoolPoint rightSpoolPoint start end

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