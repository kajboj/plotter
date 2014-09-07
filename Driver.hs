import Plotter.Driver
import Plotter.CommandWriter

main :: IO ()
main 
 = do 
  commandWriter $ hpglToCommands [PD, MV (-100, 100)]


