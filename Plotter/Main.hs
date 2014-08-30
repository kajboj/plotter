-- to do:
--
-- * change L, R to U, D
-- * make model more precise by calculating exact point on the spool where
--   string unravels
-- * split into multiple files: model, driver, parser
-- * why is driver so ugly and long?
-- * rename lines_ to someting sensible but not clashing with Prelude

module Plotter.Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import Plotter.Driver

data Step = L | R | N deriving (Eq, Show)
data Command = PenDown | PenUp | Move (Step, Step) deriving (Show)
data Pen = Up | Down deriving (Show)
data HPGLCommand = PD | PU | MV (Float, Float)

data Spool = Spool { point :: Point
                   , string :: Float
                   , angle :: Float
                   , pullSign :: Float
                   } deriving (Show)

data Plotter = Plotter { left :: Spool  
                       , right :: Spool  
                       , marker :: Point
                       , lines_ :: [(Point, Point)]
                       , pen :: Pen
                       } deriving (Show)  


leftSpool = Spool { point = (-250, 200)
                  , string = distance (-250, 200) (0, 0)
                  , angle = 0
                  , pullSign = -1}

rightSpool = Spool { point = (250, 200)
                   , string = distance (250, 200) (0, 0)
                   , angle = 0
                   , pullSign = 1}

-- nextPlotter to initialize points
plotter = nextPlotter PenUp Plotter { left = leftSpool
                                           , right = rightSpool
                                           , marker = (0, 0)
                                           , lines_ = []
                                           , pen = Up }

nextPlotter :: Command -> Plotter -> Plotter
nextPlotter command plotter@(Plotter left' right' marker' lines_' pen') = 
  Plotter { left = newLeft
          , right = newRight
          , marker = newMarker
          , lines_ = newlines_
          , pen = newPen }
  where
    newLeft = nextSpool left' leftCommand
    newRight = nextSpool right' rightCommand
    leftCommand = case command of 
      Move (l, r) -> l
      _ -> N
    rightCommand = case command of 
      Move (l, r) -> r
      _ -> N
    newMarker = intersectCircles (point newLeft) (string newLeft)
                                 (point newRight) (string newRight)
    newlines_ = case pen' of
      Up -> lines_'
      Down -> (newMarker, marker'):lines_'
    newPen = case command of
      PenDown -> Down
      PenUp -> Up
      _ -> pen'


transformPlotter :: [Command] -> Plotter -> Plotter
transformPlotter [] plotter = plotter
transformPlotter (command:coms) plotter = transformPlotter coms next
  where
    next = nextPlotter command plotter

nextSpool :: Spool -> Step -> Spool
nextSpool spool@(Spool point' string' angle' pullSign') step =
  Spool { point = point'
        , string = string' + pullPerStep * pullSign' * rotSign
        , angle = angle' + degreesPerStep * rotSign
        , pullSign = pullSign' }
  where rotSign = rotationSign step

canvasSize = (300, 200)
timePerStep = 0.002 :: Float
degreesPerStep = 1 :: Float
spoolCircumference = 2 * pi * spoolRadius
pullPerStep = (degreesPerStep / 360) * spoolCircumference
spoolRadius = 10 :: Float

main :: IO ()
main 
 =  animateIO (InWindow "Plotter" (800, 600) (5, 5))
               black
    (frame plotter)

frame :: Plotter -> Float -> IO Picture
frame plotter timeS = return pic
  where
    pic = scale $ plotterPic (transformPlotter steps plotter)
    scale = Scale 1.2 1.2
    steps = take (completeStepCount timeS) commandSequence

plotterPic :: Plotter -> Picture
plotterPic plotter = Pictures [ spoolPic (left plotter)
                              , spoolPic (right plotter)
                              , canvasPic
                              , stringPic (point $ left plotter) (marker plotter)
                              , stringPic (point $ right plotter) (marker plotter)
                              , linePic (lines_ plotter) ]

linePic :: [(Point, Point)] -> Picture
linePic lines_ = Pictures $ map renderLine lines_
  where
    renderLine (start, end) = Color white (line [start, end])

spoolPic :: Spool -> Picture
spoolPic spool = trans (point spool) (Rotate (angle spool) pic)
  where
    pic = Pictures [ Color white (circle spoolRadius)
                   , Color white (line [(0, 0), (spoolRadius, 0)]) ]

stringPic :: Point -> Point -> Picture
stringPic (spoolX, spoolY) end = Color (greyN 0.4) (line [start, end])
  where
    start = (spoolX, spoolY - spoolRadius)

trans :: Point -> Picture -> Picture
trans (x, y) pic = Translate x y pic

canvasPic :: Picture
canvasPic = color (greyN 0.2) (rectangleWire width height)
  where
    width = fst canvasSize
    height = snd canvasSize

completeStepCount :: Float -> Int
completeStepCount time = floor (time / timePerStep)

---- returns only bottom result as our strings are pulled by gravity
intersectCircles :: Point -> Float -> Point -> Float -> Point
intersectCircles (x0, y0) r0 (x1, y1) r1 = (x3, y3)
  where
    x3 = x2 + h * (y1 - y0) / d
    y3 = y2 - h * (x1 - x0) / d
    d = distance (x0, y0) (x1, y1)
    a = (r0^2 - r1^2 + d^2) / (2*d)
    h = sqrt (r0^2 - a^2)
    x2 = x0 + a * (x1 - x0) / d
    y2 = y0 + a * (y1 - y0) / d

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt (x'*x' + y'*y')
    where 
        x' = x1 - x2
        y' = y1 - y2

rotationSign :: Step -> Float
rotationSign L = -1
rotationSign R = 1
rotationSign N = 0

lengthChange :: Point -> Point -> Point -> Point -> (Float, Float)
lengthChange leftPoint rightPoint start target = (newLeft - oldLeft, newRight - oldRight)
  where
    oldLeft = distance leftPoint start
    oldRight = distance rightPoint start
    newLeft = distance leftPoint target
    newRight = distance rightPoint target 

calculateSteps :: Point -> Point -> Point -> Point -> [(Step, Step)]
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


hpgl = [ PD
       , MV (-150, -100)
       , PU
       , MV (-150, 100)
       , PD
       , MV (-75, 100)
       , MV (0, -100)
       , MV (75, 100)
       , MV (150, 100)
       , MV (150, -100)
       , MV (-100, -100) ]

hpglToCommands :: [HPGLCommand] -> [Command]
hpglToCommands hpglCommands = hpglToCommands' (0, 0) hpglCommands
  where
    hpglToCommands' prev [] = []
    hpglToCommands' prev (hpglCommand:coms) = (commands ++ hpglToCommands' newPrev coms)
      where
        commands = case hpglCommand of
          PD -> [PenDown]
          PU -> [PenUp]
          MV point -> lines_teps prev point
        newPrev = case hpglCommand of
          MV point -> point
          _ -> prev

commandSequence = hpglToCommands hpgl

lines_teps :: Point -> Point -> [Command]
lines_teps start end = map Move steps
  where
    steps = calculateSteps (point leftSpool) (point rightSpool) start end