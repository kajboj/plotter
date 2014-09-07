import Plotter.Driver
import Plotter.CommandWriter


hpgl = foldl1 (++) $ replicate 10 [ PU
                                  , MV (-100, -100)
                                  , PD
                                  , MV (-100, 100)
                                  , MV (100, 100)
                                  , MV (100, -100)
                                  , MV (-100, -100) ]


main :: IO ()
main 
 = do 
  commandWriter $ hpglToCommands (0, 0) hpgl