module Plotter.CommandWriter (commandWriter) where

import Plotter.Command
import Control.Concurrent
import System.Posix.IO
import System.Posix.Types
import System.Posix.Terminal
import System.IO
import System.Serial

commandWriter :: [Command] -> IO ()
commandWriter commands = do
  pipeFd <- initializePipeWriter
  serialHandle <- initializeSerialWriter
  putCommands pipeFd serialHandle commands
  closeFd pipeFd
  hClose serialHandle

initializePipeWriter :: IO Fd
initializePipeWriter = do
  fd <- openFd path WriteOnly Nothing defaultFileFlags
  return fd
  where
    path = "input"

initializeSerialWriter :: IO Handle
initializeSerialWriter = do
  handle <- openSerial "/dev/ttyUSB0" B115200 8 One Even Software
  putStrLn "waiting"
  threadDelay 2000000
  putStrLn "done waiting"
  return handle

putCommands :: Fd -> Handle -> [Command] -> IO ()
putCommands _ _ [] = return ()
putCommands pipeFd serialHandle (cmd:rest) = do
  putCommandToPipe pipeFd cmd
  putCommandToSerial serialHandle cmd
  putCommands pipeFd serialHandle rest

putCommandToPipe :: Fd -> Command -> IO ()
putCommandToPipe fd command = do
  fdWrite fd $ toString command
  --putStrLn $ toString command
  return ()

putCommandToSerial :: Handle -> Command -> IO ()
putCommandToSerial handle command = do
  hPutChar handle $ toChar command
  hFlush handle
  c <- hGetLine handle
  hFlush handle
  putStrLn c

toString :: Command -> String
toString command = [toChar command] ++ "\n"

toChar :: Command -> Char
toChar command = case command of
  PenUp       -> 'a'
  PenDown     -> 'b'
  Move (L, N) -> 'c'
  Move (N, L) -> 'd'
  Move (R, N) -> 'e'
  Move (N, R) -> 'f'
  Move (L, R) -> 'g'
  Move (R, L) -> 'h'
  Move (L, L) -> 'i'
  Move (R, R) -> 'j'
  _           -> ' '