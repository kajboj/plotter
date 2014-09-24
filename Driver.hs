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
    commands hpglCommands = hpglToCommands (0, 0) (0, 0, 0, 0) hpglCommands
