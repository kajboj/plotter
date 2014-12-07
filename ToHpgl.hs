import System.Environment
import Data.List

type Point = (Int, Int)
type Bounds = (Int, Int)

main = do
  input <- getLine
  let points = scale $ magnify $ parse input
      maxX   = maxi fst points
      maxY   = maxi snd points
    in
      putStrLn $ envelope (max maxX maxY) $ format points

magnify :: [(Float, Float)] -> [Point]
magnify = map (\(x, y) -> (round (x*1000), round (y*1000)))

format :: [Point] -> String
format = (intercalate ",") . map f
  where
    f (x, y) = show x ++ "," ++ show y

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

envelope :: Int -> String -> String
envelope max payload =
  "SC0," ++ show max ++ ",0," ++ show max ++ ";PU;PA0,0;PD;PA" ++
    payload ++ ";PU;PA0,0"
