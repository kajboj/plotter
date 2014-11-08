import Graphics.Gloss as Gloss
import Plotter.RasterDriver as RD
import Plotter.PngParser
import Plotter.Traversal
import System.Environment
import System.Random
import Plotter.HpglCommand
import Plotter.HpglToLines
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
  picture@((width, height), trav) <- parsePng (traversal rndGen $ args !! 1) (args !! 2)
  putStrLn $ "backtracks (pen lifts) = " ++ (show $ countBacktracks trav)
  let pixRenderer = pixelRenderer $ head args
    in display
      (InWindow "Raster graphics sim" (wndWidth, wndHeight) (0, 0))
      white
      (glossPicture width height $ hpglToLines $ hpglPicture picture pixRenderer rndGen)

hpglPicture :: RD.Picture -> RD.PixelRenderer -> StdGen -> [HPGLCommand]
hpglPicture picture pixRenderer rndGen = fst $ runState picRenderer rndGen
  where
    picRenderer = drawPic picture pixRenderer

glossPicture :: Int -> Int -> [[(Float, Float)]] -> Gloss.Picture
glossPicture width height lines =
  zoom width height $ Gloss.Color black $ Pictures (map line lines)

zoom :: Int -> Int -> Gloss.Picture -> Gloss.Picture
zoom width height = Scale scale scale . Translate (-sW/2) (-sH/2)
  where
    scale = (min (tW/sW) (tH/sH))/1.1
    tW = toF wndWidth
    tH = toF wndHeight
    sW = toF width
    sH = toF height
