import Graphics.Gloss as Gloss
import Plotter.RasterDriver as RD
import Plotter.PngParser
import System.Environment
import System.Random
import Plotter.HpglCommand
import Control.Monad.State

wndWidth  = 683 :: Int
wndHeight = 768 :: Int

toF = fromIntegral

main = do
  args <- getArgs
  image <- parsePng (head args)
  rndGen <- getStdGen
  let traversal@(width, height, _) = rowTraversal image
    in display
      (InWindow "Raster graphics sim" (wndWidth, wndHeight) (0, 0))
      white
      (glossPicture width height $ stripHpgl $ hpglPicture traversal rndGen)

hpglPicture :: Traversal -> StdGen -> [HPGLCommand]
hpglPicture traversal rndGen = fst $ runState renderer rndGen
  where
    renderer = drawPic traversal

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
