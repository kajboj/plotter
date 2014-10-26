import Plotter.Driver
import Plotter.RasterDriver
import Plotter.CommandWriter
import Plotter.Shared

main :: IO ()
main = do
  commandWriter $ commands (gradient 256 256)
  where
    commands hpglCommands = hpglToCommands (x1, y1) (0, 0, 0, 0) hpglCommands
    (x1, x2, y1, y2) = bounds