module Plotter.RasterDriver where
import Plotter.HpglCommand

maxIntensity = 256
maxPD = 10

gradient :: Int -> Int -> [HPGLCommand]
gradient width height = prefix width height ++ body ++ suffix
  where
    body = [0..(width-1)] >>= hpgl
    hpgl i = [PU, MV (fromIntegral i, 0)] ++ map (\x -> PD) [1..i]

prefix :: Int -> Int -> [HPGLCommand]
prefix width height = [SC (0, fromIntegral width, 0, fromIntegral height)]

suffix :: [HPGLCommand]
suffix = [PU, MV (0, 0)]


intensity :: Int -> [HPGLCommand]
intensity i = []