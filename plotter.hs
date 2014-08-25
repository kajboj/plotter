import Graphics.Gloss

type Size = (Float, Float)
data Step = L | R | N deriving (Eq, Show)

data Spool = Spool { point :: Point
                   , string :: Float
                   , angle :: Float
                   , steps :: [Step]
                   , pullSign :: Float
                   } deriving (Show)

data Plotter = Plotter { left :: Spool  
                       , right :: Spool  
                       , marker :: Point
                       , points :: [Point]
                       } deriving (Show)  

leftSpool = Spool { point = (-250, 200)
                  , string = 400
                  , angle = 0
                  , steps = map fst doubleSteps
                  , pullSign = -1}

rightSpool = Spool { point = (250, 200)
                   , string = 300
                   , angle = 0
                   , steps = map snd doubleSteps
                   , pullSign = 1}

plotter = Plotter { left = leftSpool
                  , right = rightSpool
                  , marker = intersectCircles (point leftSpool)
                                              (string leftSpool)
                                              (point rightSpool)
                                              (string rightSpool)
                  , points = [] }

doubleSteps = [ (L, R)
              , (R, L)
              , (R, R) 
              , (N, R)
              , (L, R)
              , (N, R) 
              , (R, R)
              , (R, R)
              , (R, R) 
              , (L, R)
              , (L, R)
              , (N, R) ]

nextPlotter :: Plotter -> Plotter
nextPlotter plotter@(Plotter left' right' marker' points') = 
  Plotter { left = newLeft
          , right = newRight
          , marker = intersectCircles (point newLeft)
                                      (string newLeft)
                                      (point newRight)
                                      (string newRight)
          , points = points' }
  where
    newLeft = nextSpool left'
    newRight = nextSpool right'

transformPlotter :: Float -> Plotter -> Plotter
transformPlotter time plotter = transformPlotter' plotter completeStepCount
  where
    transformPlotter' plotter 0 = plotter
    transformPlotter' plotter n = transformPlotter' (nextPlotter plotter) (n-1)
    (completeStepCount, _) = splitTime time

nextSpool :: Spool -> Spool
nextSpool spool@(Spool { point = _
                       , string = _
                       , angle = _
                       , steps = []
                       , pullSign = _}) = spool

nextSpool spool@(Spool point' string' angle' steps' pullSign') =
  Spool { point = point'
        , string = string' + pullPerStep * pullSign' * rotSign
        , angle = angle' + degreesPerStep * rotSign
        , steps = tail steps'
        , pullSign = pullSign' }
  where
    step = head steps'
    rotSign = rotationSign step

canvasSize = (300, 200)
timePerStep = 1 :: Float
degreesPerStep = 45 :: Float
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
                              , stringPic (point $ right plotter) (marker plotter) ]

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
