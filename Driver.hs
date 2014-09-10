import Plotter.Driver
import Plotter.CommandWriter
import Plotter.Shared
import Plotter.HpglParser


hpgl = foldl1 (++) $ replicate 10 [ PU
                                  , SC (-300, 300, -300, 300)
                                  , MV (-100, -100)
                                  , PD
                                  , MV (-100, 100)
                                  , MV (100, 100)
                                  , MV (100, -100)
                                  , MV (-100, -100) 
                                  ]


main :: IO ()
main 
 = do 
  s <- getContents
  case parseHPGL s of
    Left error -> putStrLn (show error)
    Right hpglCommands ->
      commandWriter $ hpglToCommands (0, 0) (0, 0, 0, 0) hpglCommands
