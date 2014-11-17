module Plotter.CommandReader (getCommands) where

import Plotter.Command
import Control.Applicative
import System.IO

getCommands :: Int -> IO [Command]
getCommands 0 = return []
getCommands count = (:) <$> readCommand <*> getCommands (count-1)

readCommand :: IO Command
readCommand = do
  eof <- isEOF
  if eof
    then return nop
    else do
      line <- getLine
      putStrLn line
      return $ toCommand line

nop :: Command
nop = Move (N, N)

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
  _   -> nop
