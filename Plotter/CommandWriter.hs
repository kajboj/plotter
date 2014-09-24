module Plotter.CommandWriter (commandWriter) where

import Plotter.Command
import System.Posix.IO
import System.Posix.Types

commandWriter :: [Command] -> IO ()
commandWriter commands = do
  fd <- initializeWriter
  putCommands fd commands
  closeFd fd

initializeWriter :: IO Fd
initializeWriter = do
  fd <- openFd path WriteOnly Nothing defaultFileFlags
  return fd
  where
    path = "input"

putCommands :: Fd -> [Command] -> IO ()
putCommands _ [] = return ()
putCommands fd (cmd:rest) = do
  putCommand fd cmd
  putCommands fd rest

putCommand :: Fd -> Command -> IO ()
putCommand fd command = do
  fdWrite fd (toString command)
  --putStrLn $ toString command
  return ()

toString :: Command -> String
toString command = string ++ "\n"
  where
    string = case command of
      PenUp       -> "a"
      PenDown     -> "b"
      Move (L, N) -> "c"
      Move (N, L) -> "d"
      Move (R, N) -> "e"
      Move (N, R) -> "f"
      Move (L, R) -> "g"
      Move (R, L) -> "h"
      Move (L, L) -> "i"
      Move (R, R) -> "j"
      _           -> ""