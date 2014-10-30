module Plotter.RasterDriver (drawPic, testImage) where
import Plotter.HpglCommand
import Data.List
import System.Random
import Control.Monad.State

maxIntensity = 255 :: Int
maxZigs = 1 :: Int
spikeCount = 6 :: Int
toF = fromIntegral

testImage :: [[Int]]
testImage = [ [255,  16,  16,  16]
            , [16,  255,  16,  16]
            , [16,   16, 255,  16]
            , [16,   16,  16, 255]]

rotateClockwise :: [[Int]] -> [[Int]]
rotateClockwise = reverse . rotateAntiClockwise

rotateAntiClockwise :: [[Int]] -> [[Int]]
rotateAntiClockwise ([]:_) = []
rotateAntiClockwise rows = (heads rows:(rotateAntiClockwise $ tails rows))
  where
    heads = map head
    tails = map tail

drawPic :: [[Int]] -> State StdGen [HPGLCommand]
drawPic rows = do
  b <- liftM concat $ sequence body
  return $ pref ++ b ++ suffix
  where
    pref = prefix (length $ head rows') (length rows')
    rows' = rotateClockwise rows
    body = map (\(i, row) -> drawRow i row) (zipIndex rows')

drawRow :: Int -> [Int] -> State StdGen [HPGLCommand]
drawRow rowIndex colors = liftM concat $ sequence $ map pixel (dir $ zipIndex colors)
  where
    dir = if even rowIndex then id else reverse
    pixel (i, color) = liftM ((move i) ++) (drawGray1 (point i) (normal color))
    move i = [MV $ point i]
    point i = (toF i, toF rowIndex)
    normal color = toF color / toF maxIntensity

drawGray0 :: (Float, Float) -> Float -> State StdGen [HPGLCommand]
drawGray0 (x, y) color = liftM (map MV) coords
  where
    coords = mapM randomSpike (replicate spikeCount (1-color)) >>= (return . flip (>>=) applySpike)
    applySpike (x1, y1) = [(x+x1, y+y1), (x,y)]

drawGray1 :: (Float, Float) -> Float -> State StdGen [HPGLCommand]
drawGray1 (x, y) color = liftM (map MV) coords
  where
    coords = mapM randomSpike (replicate spikeCount (1-color)) >>= (return . flip (>>=) applySpike)
    applySpike (x1, y1) = [(x+x1, y+y1)]

-- There is a bug in this implementation - spike is never normalized
-- to length equal to color intensity.
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

zipIndex :: [a] -> [(Int, a)]
zipIndex = zip [0..]