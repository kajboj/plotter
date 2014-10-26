module Plotter.RasterDriver where
import Plotter.HpglCommand
import Data.List

maxPD = 300 :: Int
toF = fromIntegral

gradient :: Int -> Int -> [HPGLCommand]
gradient width height = prefix width height ++ body ++ suffix
  where
    body = [0..(width-1)] >>= hpgl
    hpgl i = (move i) ++ (penDown i)
    move i = [PU, MV (toF i, 0)]
    penDown i = take (pdCount (toF i/toF width)) $ repeat PD

pdCount :: Float -> Int
pdCount x = round (fromAscii table x * toF maxPD)

prefix :: Int -> Int -> [HPGLCommand]
prefix width height = [SC (0, toF width, 0, toF height)]

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