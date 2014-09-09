module Plotter.HpglParser (parseHPGL) where

import Text.ParserCombinators.Parsec

data HPGLCommand = PD | PU | MV (Float, Float) |
  SC (Float, Float, Float, Float) deriving Show

hpglFile = endBy cmd (char ';')
cmd = try (string "PU" >> return [Just PU])
  <|> try (string "PD" >> return [Just PD])
  <|> try (string "PA" >> moves)
  <|> try (string "SC" >> scale)
  <|> ((many (noneOf ";")) >> return [Nothing])

moves = sepBy pair (char ',')

pair = do
  x <- many digit
  char ','
  y <- many digit
  return (Just $ MV (read x, read y))

scale = do
  xMin <- many digit
  char ','
  xMax <- many digit
  char ','
  yMin <- many digit
  char ','
  yMax <- many digit
  return [Just $ SC (read xMin, read xMax, read yMin, read yMax)]

parseHPGL :: String -> Either ParseError [HPGLCommand]
parseHPGL input = fmap (filter . flatten) (parse hpglFile "(unknown)" input)
  where
    flatten = foldl1 (++)
    filter = foldr rejectNothings []
      where
        rejectNothings e a = case e of 
          Nothing -> a
          Just cmd -> (cmd:a)