-- to do:
--
-- * change L, R to U, D
-- * make model more precise by calculating exact point on the spool where
--   string unravels
-- * split into multiple files: model, driver, parser
-- * why is driver so ugly and long?

{-# LANGUAGE DeriveDataTypeable #-}

module Plotter.Main (main) where

import Plotter.Shared
import Plotter.Driver
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.IO.Storage

canvasSize = (300, 300)

data Pen = Up | Down deriving (Show)

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
                       } deriving (Show, Typeable)


leftSpool = Spool { point = (-250, 200)
                  , string = distance (-250, 200) (0, 0)
                  , angle = 0
                  , pullSign = -1}

rightSpool = Spool { point = (250, 200)
                   , string = distance (250, 200) (0, 0)
                   , angle = 0
                   , pullSign = 1}

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
       putValue "global" "commands" commandSequence
       animateIO (InWindow "Plotter" (800, 600) (5, 5))
                 black
         (frame (getValue "global" "plotter")
                (putValue "global" "plotter")
                (commandGetter (getValue "global" "commands")
                               (putValue "global" "commands")))

type Get a = IO (Maybe a)
type Set a = a -> IO ()

commandGetter :: Get [Command] -> Set [Command] -> IO Command
commandGetter getCommands setCommands = do
  maybe <- getCommands
  case maybe of
    Just (command:coms) -> do
      setCommands coms
      return command
    _ -> return $ Move (N, N)

frame :: Get Plotter -> Set Plotter -> IO Command -> Float -> IO Picture
frame getPlotter setPlotter getCommand timeS = do
  maybe <- getPlotter
  command <- getCommand
  setPlotter $ nextPlotter command (plotter maybe)
  return $ pic $ nextPlotter command (plotter maybe)

    where
      pic plotter = scale $ plotterPic plotter
      scale = Scale 1.2 1.2
      plotter maybe = case maybe of
        Just p -> p
        Nothing -> nextPlotter PenUp Plotter { left = leftSpool
                                             , right = rightSpool
                                             , marker = (0, 0)
                                             , lines_ = []
                                             , pen = Up }

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

rotationSign :: Step -> Float
rotationSign L = -1
rotationSign R = 1
rotationSign N = 0