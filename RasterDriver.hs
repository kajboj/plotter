import Plotter.Driver
import Plotter.RasterDriver
import Plotter.CommandWriter
import Plotter.Shared
import Plotter.PngParser
import Plotter.Traversal
import System.Environment
import System.Random
import Control.Monad.State

main :: IO ()
main = do
  args <- getArgs
  rndGen <- getStdGen
  picture <- parsePng (traversal rndGen $ args !! 1) (args !! 2)
  let pixRenderer = (pixelRenderer (args !! 0))
    in commandWriter $ commands (fst $ runState (drawPic picture pixRenderer) rndGen)
  where
    commands hpglCommands = hpglToCommands (x1, y1) (0, 0, 0, 0) hpglCommands
    (x1, x2, y1, y2) = bounds
