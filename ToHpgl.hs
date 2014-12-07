import System.Environment
import Data.List

type Point = (Int, Int)
type Bounds = (Int, Int)

main = do
  input <- getLine
  let points = scale $ magnify $ parse input
    in
      putStrLn $ format points

magnify :: [(Float, Float)] -> [Point]
magnify = map (\(x, y) -> (round (x*1000), round (y*1000)))

scale :: [Point] -> [Point]
scale ps = map toBottom ps
  where
    toBottom (x, y) = (x-minX, y-minY)
    minX = mini fst ps
    minY = mini snd ps

maxi :: (Point -> Int) -> [Point] -> Int
maxi f = maximum . (map f)

mini :: (Point -> Int) -> [Point] -> Int
mini f = minimum . (map f)

parse :: String -> [(Float, Float)]
parse = read

format :: [Point] -> String
format points =
  "SC0," ++ show maximum ++ ",0," ++ show maximum ++ ";PU;PA" ++
    formatPoint (head points) ++ ";PD;PA" ++
    format (tail points) ++ ";PU;PA0,0"

  where
    format = (intercalate ",") . map formatPoint
    formatPoint (x, y) = show x ++ "," ++ show y

    maximum = max maxX maxY
    maxX = maxi fst points
    maxY = maxi snd points
