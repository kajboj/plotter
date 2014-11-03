module Plotter.PngParser (parsePng, testImage) where
import Codec.Picture

maxIntensity = 255 :: Float
toF = fromIntegral

type Traversal = ((Int, Int) -> [(Int, Int)])
type Picture = ((Int, Int), [((Float, Float), Float)])

parsePng :: Traversal -> String -> IO Picture
parsePng traversal filepath = do
  ei <- readPng filepath
  case ei of
    Left err -> error err
    Right dynamicImage -> return $ process traversal dynamicImage

process :: Traversal -> DynamicImage -> Picture
process traversal (ImageRGB8 image) = (dimensions, map gray (traversal dimensions))
  where
    dimensions@(width, height) = (imageWidth image, imageHeight image)
    gray (row, col) = case pixelAt image row (width - col - 1) of
      PixelRGB8 r g b -> ((toF row, toF col), (avg r g b) / maxIntensity)
    avg a b c = (sum $ map fromIntegral [a, b, c]) / 3

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
