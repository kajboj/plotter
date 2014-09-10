module Plotter.HpglParser (parseHPGL) where

import Text.ParserCombinators.Parsec

data HPGLCommand = PD | PU | MV (Float, Float) |
  SC (Float, Float, Float, Float) deriving Show

hpglFile = sepBy cmd (oneOf ";\n")
cmd = try (string "PU" >> return [Just PU])
  <|> try (string "PD" >> return [Just PD])
  <|> try (string "PA" >> moves)
  <|> try (string "SC" >> scale)
  <|> ((many (noneOf ";")) >> return [Nothing])

moves = sepBy pair (char ',')

pair = do
  x <- fmap read $ many digit
  char ','
  y <- fmap read $ many digit
  return (Just $ MV (x, y))

scale = do
  a <- sepBy (fmap read $ many digit) (char ',')
  return [Just $ SC ( a !! 0
                    , a !! 1
                    , a !! 2
                    , a !! 3 )]

parseHPGL :: String -> Either ParseError [HPGLCommand]
parseHPGL input = fmap (filter . flatten) (parse hpglFile "(unknown)" input)
  where
    flatten = foldl1 (++)
    filter = foldr rejectNothings []
      where
        rejectNothings e a = case e of 
          Nothing -> a
          Just cmd -> (cmd:a)

main = do
  s <- getContents
  case parseHPGL s of
    Left error -> putStrLn (show error)
    Right hpglCommands -> putStrLn (show hpglCommands)
