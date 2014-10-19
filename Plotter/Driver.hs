module Plotter.Driver (hpglToCommands, HPGLCommand(PU, PD, MV, SC)) where
import Plotter.Command
import Plotter.Shared
import Plotter.HpglCommand
import Data.List (minimumBy)

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

--lineSteps :: MyPoint -> MyPoint -> Line
--lineSteps start end = (map Move steps, endPoint)
--  where
--    (steps, endPoint) = calculateSteps1 start end (distanceToLine start end)

lineSteps :: MyPoint -> MyPoint -> Line
lineSteps start end = (map Move steps, actualEndPoint start steps)
  where
    (steps, _) = calculateSteps1 start end (distanceToLine start end)

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


makeStep :: MyPoint -> (Step, Step) -> MyPoint
makeStep start steps = intersectCircles leftSpoolPoint newLeftRadius
  rightSpoolPoint newRightRadius
  where
    (newLeftRadius, newRightRadius) = getHPair $ (+) <$> delta <*> startRadiuses
    startRadiuses = distance start <$> hPair (leftSpoolPoint, rightSpoolPoint)
    delta = (*) <$> (hPair pullSigns) <*> pulls
    pulls = (*pullPerStep) . rotationSign <$> (hPair $ steps)

calculateSteps1:: MyPoint -> MyPoint -> (MyPoint -> Float) -> ([(Step, Step)], MyPoint)
calculateSteps1 start target distToLine = case chooseSteps start target distToLine of 
                                            Nothing -> ([], start)
                                            Just (steps, newPoint) -> let (restOfSteps, endPoint) = calculateSteps1 newPoint target distToLine
                                                                      in ((steps:restOfSteps), endPoint)

chooseSteps :: MyPoint -> MyPoint -> (MyPoint -> Float) -> Maybe ((Step, Step), MyPoint)
chooseSteps start target distToLine = closestToLine (closerToTarget newPoints)
  where
    newPoints = zip possibleSteps $ map (makeStep start) possibleSteps
    possibleSteps = [(L, L),(L, N),(L, R),(N, L), (N, R),(R, L),(R, N),(R, R)]
    closerToTarget = filter (\ (_, p) -> startDistance > distance p target)
    closestToLine [] = Nothing
    closestToLine ps = Just $ minimumBy compareDistanceToLine ps
    compareDistanceToLine (_, p1) (_, p2) =
      compare (distToLine p1) (distToLine p2)
    startDistance = distance start target

distanceToLine :: MyPoint -> MyPoint -> MyPoint -> Float
distanceToLine (x1, y1) (x2, y2) (x, y) = (abs $ a*x + b*y + c) / (sqrt $ a^2 + b^2)
  where
    a = y1-y2
    b = x2-x1
    c = (x1-x2)*y1 + (y2-y1)*x1