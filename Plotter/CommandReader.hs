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
getCommands = getCommands' 6 
  where
    getCommands' 0 _ = return []
    getCommands' n fd =
      --cmd <- getCommand fd
      --rest <- getCommands' (n-1) fd
      --return (cmd:rest)
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
      putStrLn line
      return $ toCommand line

toCommand :: String -> Command
toCommand s = case s of
  "u"  -> PenUp
  "d"  -> PenDown
  "ln" -> Move (L, N)
  "nl" -> Move (N, L)
  "rn" -> Move (R, N)
  "nr" -> Move (N, R)
  "lr" -> Move (L, R)
  "rl" -> Move (R, L)
  "ll" -> Move (L, L)
  "rr" -> Move (R, R)
  _    -> Move (N, N)