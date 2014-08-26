-- to do:
--
-- * change L, R to U, D
-- * make model more precise by calculating exact point on the spool where
--   string unravels
-- * split into multiple files: model, driver, parser
-- * why is driver so ugly and long?

import Graphics.Gloss

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
                       , points :: [Point]
                       , commands :: [Command]
                       , pen :: Pen
                       } deriving (Show)  


leftSpool = Spool { point = (-250, 200)
                  , string = 400
                  , angle = 0
                  , pullSign = -1}

rightSpool = Spool { point = (250, 200)
                   , string = 300
                   , angle = 0
                   , pullSign = 1}

-- nextPlotter to initialize points
plotter = nextPlotter Plotter { left = leftSpool
                              , right = rightSpool
                              , marker = (0, 0)
                              , points = []
                              , commands = commandSequence
                              , pen = Up }

commandSequence = [PenDown] ++ map Move (foldl1 (++) (map calc lines))
  where
    calc (x, y) = calculateSteps (point leftSpool) (point rightSpool) x y
    lines = zip points (tail points)
    initialPoint = intersectCircles (point leftSpool)
                                    (string leftSpool)
                                    (point rightSpool)
                                    (string rightSpool)
    points = [ initialPoint
             , (-150, -100)
             , (-150, 100)
             , (-75, 100)
             , (0, -100)
             , (75, 100)
             , (150, 100)
             , (150, -100)
             , (-150, -100) ]

nextPlotter :: Plotter -> Plotter
nextPlotter plotter@(Plotter left' right' marker' points' [] pen) = plotter

nextPlotter plotter@(Plotter left' right' marker' points' (command:coms) pen') = 
  Plotter { left = newLeft
          , right = newRight
          , marker = newMarker
          , points = newPoints
          , commands = coms
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
    newPoints = case pen' of
      Up -> points'
      Down -> newMarker:points'
    newPen = case command of
      PenDown -> Down
      PenUp -> Up
      _ -> pen'


transformPlotter :: Float -> Plotter -> Plotter
transformPlotter time plotter = transformPlotter' plotter completeStepCount
  where
    transformPlotter' plotter 0 = plotter
    transformPlotter' plotter n = transformPlotter' (nextPlotter plotter) (n-1)
    (completeStepCount, _) = splitTime time

nextSpool :: Spool -> Step -> Spool
nextSpool spool@(Spool point' string' angle' pullSign') step =
  Spool { point = point'
        , string = string' + pullPerStep * pullSign' * rotSign
        , angle = angle' + degreesPerStep * rotSign
        , pullSign = pullSign' }
  where
    rotSign = rotationSign step

canvasSize = (300, 200)
timePerStep = 0.002 :: Float
degreesPerStep = 1 :: Float
spoolCircumference = 2 * pi * spoolRadius
pullPerStep = (degreesPerStep / 360) * spoolCircumference
spoolRadius = 10 :: Float

main :: IO ()
main 
 =  animate (InWindow "Plotter" (800, 600) (5, 5))
                black
    frame 

frame :: Float -> Picture
frame timeS = Scale 1.2 1.2
  $ plotterPic (transformPlotter timeS plotter)

plotterPic :: Plotter -> Picture
plotterPic plotter = Pictures [ spoolPic (left plotter)
                              , spoolPic (right plotter)
                              , canvasPic
                              , stringPic (point $ left plotter) (marker plotter)
                              , stringPic (point $ right plotter) (marker plotter)
                              , linePic (points plotter) ]

linePic :: [Point] -> Picture
linePic points = Color white (line points)

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

splitTime :: Float -> (Int, Float)
splitTime time = (completeStepCount, remainder)
  where
    steps = time / timePerStep
    completeStepCount = floor steps
    remainder = (time - (fromIntegral completeStepCount) * timePerStep) / timePerStep

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
