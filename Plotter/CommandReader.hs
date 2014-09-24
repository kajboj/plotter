module Plotter.CommandReader (initializeReader, getCommands) where

import Plotter.Command
import System.Posix.Files
import System.IO
import Control.Applicative

initializeReader :: IO Handle
initializeReader = do
  fd <- openFile path ReadMode
  hSetBuffering fd LineBuffering
  return fd

  where
    path = "input"

getCommands :: Handle -> IO [Command]
getCommands = getCommands' 20 
  where
    getCommands' 0 _ = return []
    getCommands' n fd =
      (:) <$> (getCommand fd) <*> (getCommands' (n-1) fd)

getCommand :: Handle -> IO Command
getCommand fd = do
  isEof <- hIsEOF fd
  if isEof
    then do
      --putStrLn "no input"
      return $ Move (N, N)
    else do
      line <- hGetLine fd
      --putStrLn line
      return $ toCommand line

toCommand :: String -> Command
toCommand s = case s of
  "a" -> PenUp
  "b" -> PenDown
  "c" -> Move (L, N)
  "d" -> Move (N, L)
  "e" -> Move (R, N)
  "f" -> Move (N, R)
  "g" -> Move (L, R)
  "h" -> Move (R, L)
  "i" -> Move (L, L)
  "j" -> Move (R, R)
  _   -> Move (N, N)