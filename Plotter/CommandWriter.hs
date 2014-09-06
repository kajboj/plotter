module Plotter.CommandWriter (commandWriter, main) where

import Plotter.Command
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types
import System.IO
import System.Directory

commandWriter :: Command -> IO ()
commandWriter command = do
  fd <- initializeWriter
  putCommand fd command

main :: IO ()
main 
 = do 
  commandWriter $ Move (L, N)


initializeWriter :: IO Fd
initializeWriter = do
  fd <- openFd path WriteOnly Nothing defaultFileFlags
  return fd
  where
    path = "input"

putCommand :: Fd -> Command -> IO ()
putCommand fd command = do
  fdWrite fd (toString command)
  putStrLn $ toString command

toString :: Command -> String
toString command = string ++ "\n"
  where
    string = case command of
      PenUp       -> "u"
      PenDown     -> "d"
      Move (L, N) -> "ln"
      Move (N, L) -> "nl"
      Move (R, N) -> "rn"
      Move (N, R) -> "nr"
      Move (L, R) -> "lr"
      Move (R, L) -> "rl"
      Move (L, L) -> "ll"
      Move (R, R) -> "rr"
      _           -> "nn"