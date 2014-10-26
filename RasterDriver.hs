import Plotter.Driver
import Plotter.RasterDriver
import Plotter.CommandWriter
import Plotter.Shared
import Plotter.PngParser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  image <- parsePng (head args)
  commandWriter $ commands (drawPic image)
  where
    commands hpglCommands = hpglToCommands (x1, y1) (0, 0, 0, 0) hpglCommands
    (x1, x2, y1, y2) = bounds