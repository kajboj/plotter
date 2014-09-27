import Plotter.Driver
import Plotter.CommandWriter
import Plotter.Shared
import Plotter.HpglParser

main :: IO ()
main = do
  s <- getContents
  case parseHPGL s of
    Left error -> putStrLn (show error)
    Right hpglCommands -> do
      commandWriter $ commands hpglCommands
  where
    commands hpglCommands = hpglToCommands (x1, y1) (0, 0, 0, 0) hpglCommands
    (x1, x2, y1, y2) = bounds