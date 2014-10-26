{-# LANGUAGE DeriveDataTypeable #-}

module Plotter.Device (main) where

import Plotter.Shared
import Plotter.Command
import Plotter.CommandReader
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.IO.Storage
import Data.Typeable


initialPosition = (x1, y1)
  where
    (x1, x2, y1, y2) = bounds

inkDispersion = 0.008

data Dot = Dot Int Point deriving Show

data Pen = Up | Down deriving (Show)

data Spool = Spool { point :: Point
                   , string :: Float
                   , angle :: Float
                   , pullSign :: Float
                   } deriving (Show)

data Plotter = Plotter { left :: Spool  
                       , right :: Spool  
                       , marker :: Point
                       , dots :: [Dot]
                       , lines_ :: [[Point]]
                       , pen :: Pen
                       } deriving (Show, Typeable)


leftSpool = Spool { point = leftSpoolPoint
                  , string = distance leftSpoolPoint initialPosition
                  , angle = 0
                  , pullSign = fst pullSigns}

rightSpool = Spool { point = rightSpoolPoint
                   , string = distance rightSpoolPoint initialPosition
                   , angle = 0
                   , pullSign = snd pullSigns}

nextPlotter :: Plotter -> Command -> Plotter
nextPlotter plotter (Move (N, N)) = plotter

nextPlotter plotter@(Plotter left right marker dots lines_ pen) PenUp = 
  Plotter left right marker dots lines_ Up

nextPlotter plotter@(Plotter left right marker dots lines_ pen) PenDown = 
  Plotter left right marker newDots ([]:lines_) Down
  where
    newDots = case dots of
      [] -> [Dot 1 marker]
      ((Dot i point):rest) -> if point == marker
        then ((Dot (i+1) marker):rest)
        else ((Dot 1 marker):dots)

nextPlotter plotter@(Plotter left' right' marker' dots' lines_' pen') (Move (l, r)) = 
  Plotter { left = newLeft
          , right = newRight
          , marker = newMarker
          , dots = dots'
          , lines_ = newlines_
          , pen = pen' }
  where
    newLeft = nextSpool left' l
    newRight = nextSpool right' r
    newMarker = intersectCircles (point newLeft) (string newLeft)
                                 (point newRight) (string newRight)
    newlines_ = case pen' of
      Up -> lines_'
      Down -> ((newMarker):(head lines_')):(tail lines_')

nextSpool :: Spool -> Step -> Spool
nextSpool spool@(Spool point' string' angle' pullSign') step =
  Spool { point = point'
        , string = string' + pullPerStep * pullSign' * rotSign
        , angle = angle' + degreesPerStep * rotSign
        , pullSign = pullSign' }
  where rotSign = rotationSign step

main :: IO ()
main 
 = do 
   withStore "global" animation
   where
     animation = do
       fileHandle <- initializeReader
       animateIO (InWindow "Plotter" (683, 768) (0, 0))
                 black
         (frame (getValue "global" "plotter")
                (putValue "global" "plotter")
                (getCommands fileHandle))

type Get a = IO (Maybe a)
type Set a = a -> IO ()

transformPlotter :: [Command] -> Plotter -> Plotter
transformPlotter commands plotter = foldl nextPlotter plotter commands

frame :: Get Plotter -> Set Plotter -> IO [Command] -> Float -> IO Picture
frame getPlotter setPlotter getCommands timeS = do
  maybe <- getPlotter
  commands <- getCommands
  setPlotter $ transformPlotter commands (plotter maybe)
  return $ pic $ transformPlotter commands (plotter maybe)

    where
      pic plotter = scale $ trans (0, -80) $ plotterPic plotter
      scale = Scale 1 1
      plotter (Just p) = p
      plotter Nothing = nextPlotter Plotter { left = leftSpool
                                            , right = rightSpool
                                            , marker = initialPosition
                                            , dots = []
                                            , lines_ = [[]]
                                            , pen = Up }
                                            PenUp

plotterPic :: Plotter -> Picture
plotterPic plotter = Pictures [ spoolPic (left plotter)
                              , spoolPic (right plotter)
                              , canvasPic
                              , stringPic (point $ left plotter) (marker plotter)
                              , stringPic (point $ right plotter) (marker plotter)
                              , linePic (lines_ plotter)
                              , dotsPic (dots plotter)]

linePic :: [[Point]] -> Picture
linePic lines_ = Pictures $ map renderLine lines_
  where
    renderLine points = Color white (line points)

spoolPic :: Spool -> Picture
spoolPic spool = trans (point spool) (Rotate (angle spool) pic)
  where
    pic = Pictures [ Color white (circle spoolRadius)
                   , Color white (line [(0, 0), (spoolRadius, 0)]) ]

stringPic :: Point -> Point -> Picture
stringPic (spoolX, spoolY) end = Color (greyN 0.4) (line [start, end])
  where
    start = (spoolX, spoolY - spoolRadius)

dotsPic :: [Dot] -> Picture
dotsPic [] = Pictures []
dotsPic dots = Pictures (map dotPic dots)

dotPic :: Dot -> Picture
dotPic (Dot size point) = trans point $ Pictures [Color white (circle $ (fromIntegral size)*inkDispersion)]

trans :: Point -> Picture -> Picture
trans (x, y) pic = Translate x y pic

canvasPic :: Picture
canvasPic = color (greyN 0.2) (rectangleWire width height)
  where
    width = x2-x1
    height = y2-y1
    (x1, x2, y1, y2) = bounds

