import Graphics.Gloss

type Size = (Float, Float)
data Rot = L | R | N deriving (Eq, Show)
type Step = (Rot, Rot)
type History = ([Rot], Maybe Rot)

leftSpoolPoint = (-250, 200)
rightSpoolPoint = (250, 200)

initialLeftStringLength = 400
initialRightStringLength = 300

canvasSize = (300, 200)

timePerStep = 1 :: Float
degreesPerStep = 45 :: Float
spoolCircumference = 2 * pi * spoolRadius
stepPull = (degreesPerStep / 360) * spoolCircumference

spoolRadius = 10 :: Float

steps = [ (L, R)
        , (N, L)
        , (R, R) 
        , (L, R)
        , (N, L)
        , (R, R) 
        , (L, R)
        , (N, L)
        , (R, R) 
        , (L, R)
        , (N, L)
        , (R, R) ]

main :: IO ()
main 
 =  animate (InWindow "Plotter" (800, 600) (5, 5))
                black
    frame 

frame :: Float -> Picture
frame timeS = Scale 1.2 1.2
  $ Pictures [ spools timeS
             , string leftSpoolPoint marker
             , string rightSpoolPoint marker
             , canvasPic ]
  where
    marker = markerPoint initialLeftStringLength initialRightStringLength

spools :: Float -> Picture
spools timeS = Pictures
  [ trans leftSpoolPoint (Rotate degreesLeft spoolPic)
  , trans rightSpoolPoint (Rotate degreesRight spoolPic) ]
  where
    degreesLeft = spoolDegrees (leftRots steps) (splitTime timeS)
    degreesRight = spoolDegrees (rightRots steps) (splitTime timeS)

spoolDegrees :: [Rot] -> (Int, Float) -> Float
spoolDegrees rots (completeSteps, current) = steps * degreesPerStep
  where
    steps = histToSteps hist current
    hist = history rots completeSteps


trans :: Point -> Picture -> Picture
trans (x, y) pic = Translate x y pic

spoolPic :: Picture
spoolPic = Pictures [ Color white (circle spoolRadius)
                    , Color white (line [(0, 0), (spoolRadius, 0)]) ]

canvasPic :: Picture
canvasPic = color (greyN 0.2) (rectangleWire width height)
  where
    width = fst canvasSize
    height = snd canvasSize


count :: Eq a => a -> [a] -> Int
count elem list = length $ filter (\x -> x == elem) list

neutralize :: [Rot] -> Int
neutralize rots = (count R rots) - (count L rots)

history :: [Rot] -> Int -> History
history rots completeStepCount = 
  (take completeStepCount rots, elementAt rots completeStepCount)

elementAt :: [a] -> Int -> Maybe a
elementAt list index = if length list <= index
  then Nothing
  else Just $ list !! index

splitTime :: Float -> (Int, Float)
splitTime time = (completeStepCount, remainder)
  where
    steps = time / timePerStep
    completeStepCount = floor steps
    remainder = (time - (fromIntegral completeStepCount) * timePerStep) / timePerStep

histToSteps :: History -> Float -> Float
histToSteps (complete, inProgress) remainder =
  (fromIntegral $ neutralize complete) + fractional
  where
    fractional = case inProgress of
      Nothing -> 0
      Just rot -> remainder * (direction rot)

markerPoint :: Float -> Float -> Point
markerPoint left right =
  intersectCircles leftSpoolPoint left rightSpoolPoint right

-- returns only bottom result as our strings are pulled by gravity
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

string :: Point -> Point -> Picture
string (spoolX, spoolY) end = Color (greyN 0.4) (line [start, end])
  where
    start = (spoolX, spoolY - spoolRadius)

direction :: Rot -> Float
direction L = -1
direction R = 1
direction N = 0

leftRots :: [Step] -> [Rot]
leftRots = map fst

rightRots :: [Step] -> [Rot]
rightRots = map snd


 --makeColor 0.3 0.3 1.0 1.0