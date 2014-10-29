module Plotter.RasterDriver (drawPic, testImage) where
import Plotter.HpglCommand
import Data.List
import System.Random
import Control.Monad.State

maxIntensity = 255 :: Int
maxZigs = 1 :: Int
spikeCount = 4 :: Int
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
    pixel (i, color) = liftM ((move i) ++) (drawGray0 (point i) (normal color))
    move i = [MV $ point i]
    point i = (toF i, toF rowIndex)
    normal color = toF color / toF maxIntensity

drawGray0 :: (Float, Float) -> Float -> State StdGen [HPGLCommand]
drawGray0 (x, y) color = liftM (map MV) coords
  where
    coords = mapM randomSpike (replicate spikeCount color) >>= (return . flip (>>=) applySpike)
    applySpike (x1, y1) = [(x+x1, y+y1), (x,y)]

randomSpike :: Float -> State StdGen (Float, Float)
randomSpike length = liftM2 (,) (randomVal length) (randomVal length)

randomVal :: Float -> State StdGen Float
randomVal radius = do 
  rndGen <- get
  let (rndVal, rndGen') = randomR (-radius/2, radius/2) rndGen
  put rndGen'
  return rndVal

normalize :: (Float, Float) -> (Float, Float)
normalize (x, y) = (x/length, y/length)
  where
    length = sqrt $ x^2 + y^2

drawGray1 :: (Float, Float) -> Float -> [HPGLCommand]
drawGray1 (x, y) color = map MV coords
  where
    coords = map (\(x1, y1) -> (x+color*x1, y+color*y1)) [(1, 0), (1, 1), (0, 1), (0, 0)]

drawGray2 :: (Float, Float) -> Float -> [HPGLCommand]
drawGray2 (x, y) color = map MV coords
  where
    coords = [(x+color, y+color)]

drawGray3 :: (Float, Float) -> Float -> [HPGLCommand]
drawGray3 (x, y) color = map MV coords
  where
    coords = map f zigz
    f (x1, y1) = (x+x1, y+y1)
    zigz = map toF [1..zigCount] >>= zig
    zig i = [((i-1)*zigDelta, color), (i*zigDelta, 0)]
    zigDelta = 1 / (toF zigCount)
    zigCount = maxZigs

prefix :: Int -> Int -> [HPGLCommand]
prefix width height = [SC (0, toF height, 0, toF width), PD]

suffix :: [HPGLCommand]
suffix = [PU, MV (0, 0)]

zipIndex :: [a] -> [(Int, a)]
zipIndex = zip [0..]

putCurve :: [String] -> IO ()
putCurve curve = mapM_ putStrLn curve

plot :: [Int] -> [String]
plot curve = map (\n -> take n (repeat '*')) curve

scale :: Int -> [Float] -> [Int]
scale max curve = map (round . (* toF max)) curve

sampleCurve :: (Float -> Float) -> Int -> [Float]
sampleCurve f sampleCount = map (f . iToF) [0..(sampleCount-1)]
  where
   iToF i = (toF i) / (toF (sampleCount-1))

fromAscii :: [String] -> Float -> Float
fromAscii ascii x = case find (matchingRange x) (ranges $ normal ascii) of
  Just ((minX, maxX), (minY, maxY)) -> minY + ((x - minX) / (maxX - minX) * (maxY - minY))
  Nothing -> error $ "couldn't find value for " ++ show x

  where
    matchingRange x ((minX, maxX), _) = x >= minX && x <= maxX

ranges :: [Float] -> [((Float, Float), (Float, Float))]
ranges vals = zip (toRanges xs) (toRanges vals)
  where
    xs = map (norm . toF) [0..(length vals)]
    norm = (/(toF $ length vals))
    toRanges list = zip list $ (drop 1 list) ++ [last list]

normal :: [String] -> [Float]
normal ascii = map normalize lengths
  where
    lengths = map length ascii
    normalize i = (toF i) / (toF $ maximum lengths)

table = [ ""
        , "***"
        , "*****"
        , "******"
        , "*******"
        , "*********"
        , "*************"
        , "*************************"
        , "**********************************"]