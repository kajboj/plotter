module Plotter.CommandWriter (commandWriter, main) where

import Plotter.Command
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types
import System.IO
import System.Directory

commandWriter :: [Command] -> IO ()
commandWriter commands = do
  fd <- initializeWriter
  putCommands fd commands

main :: IO ()
main 
 = do 
  commandWriter $ foldl1 (++) $ replicate 10 [ PenDown
                                             , Move (L, N)
                                             , Move (L, R)
                                             , Move (N, R)
                                             , Move (R, N)
                                             , Move (R, L)
                                             , Move (N, L) ]


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