module Plotter.RasterDriver ( drawPic
                            , randomWalk
                            , randomStar
                            , justADot
                            , PixelRenderer
                            , Picture
                            , Renderer) where
import Plotter.HpglCommand
import Plotter.Traversal (Traversal, stepValue, Step(Forward, Backtrack))
import Data.List
import System.Random
import Control.Monad.State

type Color = Float
type Coords = (Float, Float)
type Pixel = (Coords, Color)
type Width = Int
type Height = Int
type Picture = ((Width, Height), Traversal Pixel)
type PixelRenderer = Pixel -> State StdGen [HPGLCommand]
type Renderer = State StdGen [HPGLCommand]

spikeCount = 6 :: Int
toF = fromIntegral

drawPic :: Picture -> (Pixel -> Renderer) -> Renderer
drawPic ((width, height), pixels) pixelRenderer  =
  liftM applyEnvelope $ body pixelRenderer pixels
  where
    applyEnvelope body = pref ++ body ++ suffix
    pref = prefix width height

body :: (Pixel -> Renderer) -> Traversal Pixel -> Renderer
body pixelRenderer pixels = liftM concat $ sequence $ map pix pixels
  where
    pix step = liftM (move step ++) (pixelRenderer $ stepValue step)
    move (Forward   (point, _)) = [MV $ point]
    move (Backtrack (point, _)) = [PU, MV $ point, PD]

justADot :: Pixel -> State StdGen [HPGLCommand]
justADot _ = return []

randomStar :: Pixel -> State StdGen [HPGLCommand]
randomStar ((x, y), color) = liftM (map MV) coords
  where
    coords = mapM randomSpike (replicate spikeCount (1-color)) >>= (return . flip (>>=) applySpike)
    applySpike (x1, y1) = [(x+x1, y+y1), (x,y)]

randomWalk :: Pixel -> State StdGen [HPGLCommand]
randomWalk ((x, y), color) = liftM (map MV) coords
  where
    coords = mapM randomSpike (replicate spikeCount (1-color)) >>= (return . flip (>>=) applySpike)
    applySpike (x1, y1) = [(x+x1, y+y1)]

randomSpike :: Float -> State StdGen (Float, Float)
randomSpike length = liftM2 (,) (randomVal length) (randomVal length)

randomVal :: Float -> State StdGen Float
randomVal radius = do
  rndGen <- get
  let (rndVal, rndGen') = randomR (-radius, radius) rndGen
  put rndGen'
  return rndVal

normalize :: (Float, Float) -> (Float, Float)
normalize (x, y) = (x/length, y/length)
  where
    length = sqrt $ x^2 + y^2

prefix :: Int -> Int -> [HPGLCommand]
prefix width height = [SC (0, toF height, 0, toF width), PD]

suffix :: [HPGLCommand]
suffix = [PU, MV (0, 0)]
