import Graphics.Gloss as Gloss
import Plotter.RasterDriver as RD
import Plotter.PngParser
import Plotter.Traversal
import System.Environment
import System.Random
import Plotter.HpglCommand
import Control.Monad.State

wndWidth  = 683 :: Int
wndHeight = 716 :: Int

toF = fromIntegral

pixelRenderer :: String -> PixelRenderer
pixelRenderer "--walk" = randomWalk
pixelRenderer "--star" = randomStar
pixelRenderer "--dot" = justADot
traversal rndGen "--random" = randomDeepTraversal rndGen
traversal rndGen "--row"    = rowByRowTraversal

main = do
  args <- getArgs
  rndGen <- getStdGen
  picture@((width, height), _) <- parsePng (traversal rndGen $ args !! 1) (args !! 2)
  let pixRenderer = pixelRenderer $ head args
    in display
      (InWindow "Raster graphics sim" (wndWidth, wndHeight) (0, 0))
      white
      (glossPicture width height $ stripHpgl $ hpglPicture picture pixRenderer rndGen)

hpglPicture :: RD.Picture -> RD.PixelRenderer -> StdGen -> [HPGLCommand]
hpglPicture picture pixRenderer rndGen = fst $ runState picRenderer rndGen
  where
    picRenderer = drawPic picture pixRenderer

glossPicture :: Int -> Int -> [(Float, Float)] -> Gloss.Picture
glossPicture width height points =
  zoom width height $ Color black (line points)

stripHpgl :: [HPGLCommand] -> [(Float, Float)]
stripHpgl hpgl = concat $ map strip hpgl
  where
    strip PD         = []
    strip PU         = []
    strip (SC _)     = []
    strip (MV point) = [point]

zoom :: Int -> Int -> Gloss.Picture -> Gloss.Picture
zoom width height = Scale scale scale . Translate (-sW/2) (-sH/2)
  where
    scale = (min (tW/sW) (tH/sH))/1.1
    tW = toF wndWidth
    tH = toF wndHeight
    sW = toF width
    sH = toF height
