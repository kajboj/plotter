module Plotter.RasterDriver (drawPic, gradient, testImage) where
import Plotter.HpglCommand
import Data.List

maxIntensity = 255 :: Int
maxPD = 300 :: Int
toF = fromIntegral

gradient :: Int -> Int -> [HPGLCommand]
gradient width height = drawPic (map (\_ -> [0..width]) [0..height])

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

drawPic :: [[Int]] -> [HPGLCommand]
drawPic rows = prefix (length $ head rows') (length rows') ++ body ++ suffix
  where
    rows' = rotateClockwise rows
    body = (zip [0..] rows') >>= \(i, row) -> drawRow i row

drawRow :: Int -> [Int] -> [HPGLCommand]
drawRow rowIndex colors = (dir $ zip [0..] colors) >>= pixel
  where
    dir = if even rowIndex then id else reverse
    pixel (i, color) = (move i) ++ (penDowns color)
    penDowns color = take (pdCount $ normal color) $ repeat PD
    move i = [PU, MV (toF i, toF rowIndex)]
    normal color = toF color / toF maxIntensity

pdCount :: Float -> Int
pdCount x = round (fromAscii table x * toF maxPD)

prefix :: Int -> Int -> [HPGLCommand]
prefix width height = [SC (0, toF height, 0, toF width)]

suffix :: [HPGLCommand]
suffix = [PU, MV (0, 0)]

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