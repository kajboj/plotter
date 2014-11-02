module Plotter.Path where

import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Set as S

type Grid a = V.Vector (V.Vector a)
type Coords = (Int, Int)
type Dimensions = (Int, Int)

marks = L.concat $ repeat "abcdefghijklmnopqrstuvyz"

path :: Dimensions -> [Coords]
path dims@(width, height) = reverse . snd $ tree dims [start] unvisited
  where
    start = (0, 0)
    unvisited = S.delete start allNodes
    allNodes = S.fromList [(r, c) | r <- [0..height-1], c <- [0..width-1]]

tree :: Dimensions -> [Coords] -> S.Set Coords -> (S.Set Coords, [Coords])
tree dims path@(node:rest) unvisited = if S.null unvisited
  then (unvisited, path)
  else foldr visit (unvisited, path) neighbours
  where
    visit n (unv, path) = tree dims (n:path) (S.delete n unv)
    neighbours = unvisitedNeighbours dims unvisited node 

unvisitedNeighbours :: Dimensions -> S.Set Coords -> Coords -> [Coords]
unvisitedNeighbours dims unvisited coords =
  filter (flip S.member unvisited) (neighbours dims coords)

neighbours :: Dimensions -> Coords -> [Coords]
neighbours (w, h) (r, c) = filter onGrid [(r+ro, c+co) | (ro, co) <- offsets]
  where
    onGrid (r, c) = (r >= 0) && (r <= h) && (c >= 0) && (c <= w)

offsets = [(-1,0),(0,-1),(0,1),(1,0)]

toString :: Dimensions -> [Coords] -> String
toString dims path = (L.intercalate eol $ list) ++ eol
  where 
    list = V.toList $ V.map V.toList $ markPath dims (zip path marks)
    eol = "\n"

markPath :: Dimensions -> [((Int, Int), Char)] -> Grid Char
markPath (width, height) = foldr mark (blank width height)
  where
    mark ((row, col), char) = set2 row col char

set :: Int -> a -> V.Vector a -> V.Vector a
set i e v = v V.// [(i, e)]

set2 :: Int -> Int -> a -> Grid a -> Grid a
set2 row col e grid = set row (set col e (grid V.! row)) grid

blank :: Int -> Int -> V.Vector (V.Vector Char)
blank width height = V.replicate height row
  where
    row = V.replicate width '-'