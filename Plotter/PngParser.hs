module Plotter.PngParser (parsePng) where
import Codec.Picture

parsePng :: String -> IO [[Int]]
parsePng filepath = do
  ei <- readPng filepath
  case ei of
    Left err -> error err
    Right dynamicImage -> return $ process dynamicImage


process :: DynamicImage -> [[Int]]
process (ImageRGB8 image) = map (map gray) coords
  where
    gray (x, y) = case pixelAt image x y of
      PixelRGB8 r _ _ -> fromIntegral r

    coords = map row [0..imageHeight image - 1]
    row i = map (\col -> (i, col)) [0..imageWidth image - 1]
