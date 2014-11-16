module Plotter.RealDevice (main) where

import Plotter.Command
import Plotter.CommandReader
import Control.Concurrent
import System.Posix.Terminal
import System.IO
import System.Serial
import Data.List

main = do
  handle <- initializeSerial
  getContents >>= mapM_ (outputChar handle) . intercalate "" . lines
  hClose handle

initializeSerial :: IO Handle
initializeSerial = do
  handle <- openSerial "/dev/ttyACM0" B115200 8 One NoParity Software
  putStrLn "waiting"
  threadDelay 2000000
  putStrLn "done waiting"
  return handle

outputChar :: Handle -> Char -> IO ()
outputChar handle char = do
  hPutChar handle char
  hFlush handle
  c <- hGetLine handle
  hFlush handle
  putStrLn c
