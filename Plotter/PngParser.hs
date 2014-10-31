module Plotter.PngParser (parsePng, testImage) where
import Codec.Picture

maxIntensity = 255 :: Float

parsePng :: String -> IO [[Float]]
parsePng filepath = do
  ei <- readPng filepath
  case ei of
    Left err -> error err
    Right dynamicImage -> return $ process dynamicImage


process :: DynamicImage -> [[Float]]
process (ImageRGB8 image) = rotateClockwise $ map (map gray) coords
  where
    gray (x, y) = case pixelAt image x y of
      PixelRGB8 r _ _ -> fromIntegral r / maxIntensity

    coords = map row [0..imageHeight image - 1]
    row i = map (\col -> (i, col)) [0..imageWidth image - 1]


testImage :: [[Float]]
testImage = rotateClockwise [ [0.0, 1.0, 1.0, 1.0]
                            , [1.0, 1.0, 1.0, 1.0]
                            , [1.0, 1.0, 1.0, 1.0]
                            , [1.0, 1.0, 1.0, 1.0]]


rotateClockwise :: [[Float]] -> [[Float]]
rotateClockwise = reverse . rotateAntiClockwise

rotateAntiClockwise :: [[Float]] -> [[Float]]
rotateAntiClockwise ([]:_) = []
rotateAntiClockwise rows = (heads rows:(rotateAntiClockwise $ tails rows))
  where
    heads = map head
    tails = map tail
