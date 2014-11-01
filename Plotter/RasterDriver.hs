module Plotter.RasterDriver (drawPic, rowTraversal, Picture, Traversal) where
import Plotter.HpglCommand
import Data.List
import System.Random
import Control.Monad.State

spikeCount = 6 :: Int
toF = fromIntegral

type Color = Float
type Picture = [[Color]]
type Coords = (Float, Float)
type Pixel = (Coords, Color)
type Width = Int
type Height = Int
type Traversal = (Width, Height, [Pixel])

rowTraversal :: Picture -> Traversal
rowTraversal pic =
  let sequence = [((toF j, toF i), color) | (i, row) <- zipI pic, (j, color) <- dir i $ zipI row ]
      dir rowIndex = if even rowIndex then id else reverse
      height = length pic
      width = length $ head pic
  in (width, height, sequence)

drawPic :: Traversal -> State StdGen [HPGLCommand]
drawPic (width, height, pixels) = liftM applyEnvelope $ body pixels
  where
    applyEnvelope body = pref ++ body ++ suffix
    pref = prefix width height

body :: [Pixel] -> State StdGen [HPGLCommand]
body pixels = liftM concat $ sequence $ map pix pixels
  where
    pix pixel@(point, color) = liftM (move point:) (randomWalk pixel)
    move point = MV $ point

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

zipI :: [a] -> [(Int, a)]
zipI = zip [0..]