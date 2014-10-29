import Plotter.Driver
import Plotter.RasterDriver
import Plotter.CommandWriter
import Plotter.Shared
import Plotter.PngParser
import System.Environment
import System.Random
import Control.Monad.State

main :: IO ()
main = do
  args <- getArgs
  image <- parsePng (head args)
  rndGen <- getStdGen
  commandWriter $ commands (fst $ runState (drawPic image) rndGen)
  --commandWriter $ commands (fst $ runState (drawPic testImage) rndGen)
  where
    commands hpglCommands = hpglToCommands (x1, y1) (0, 0, 0, 0) hpglCommands
    (x1, x2, y1, y2) = bounds