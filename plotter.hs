import Graphics.Gloss

type Size = (Float, Float)
data Rot = L | R | N deriving (Eq, Show)
type Step = (Rot, Rot)
type History = ([Rot], Maybe Rot)

leftSpoolPoint = (-250, 200)
rightSpoolPoint = (250, 200)
canvasSize = (300, 200)

timePerStep = 1 :: Float
degreesPerStep = 45 :: Float

spoolRadius = 10 :: Float

steps = [ (L, R)
        , (N, L)
        , (R, R) ]

main :: IO ()
main 
 =  animate (InWindow "Plotter" (800, 600) (5, 5))
                black
    frame 

frame :: Float -> Picture
frame timeS
 = Pictures [ spools timeS
            , canvasPic ]

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

direction :: Rot -> Float
direction L = -1
direction R = 1
direction N = 0

leftRots :: [Step] -> [Rot]
leftRots = map fst

rightRots :: [Step] -> [Rot]
rightRots = map snd


 --makeColor 0.3 0.3 1.0 1.0