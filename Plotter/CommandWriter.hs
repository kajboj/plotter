module Plotter.CommandWriter (commandWriter) where

import Plotter.Command
import Control.Concurrent
import System.Posix.IO
import System.Posix.Types
import System.Posix.Terminal
import System.IO
import System.Serial
import Control.Monad

commandWriter :: [Command] -> IO ()
commandWriter commands = do
  sequence $ map (putStrLn . toString) commands
  return ()

toString :: Command -> String
toString command = [toChar command]

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
