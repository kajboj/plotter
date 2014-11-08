{-# LANGUAGE DeriveFunctor #-}

module Plotter.Traversal ( rowByRowTraversal
                         , randomDeepTraversal
                         , stepValue
                         , TraversalGen
                         , Traversal
                         , Step (Forward, Backtrack)
                         ) where

import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Set as S
import Data.Char
import System.Random
import Control.Monad.State

type Grid a = V.Vector (V.Vector a)
type Coords = (Int, Int)
type Dimensions = (Int, Int)
type TraversalGen = Dimensions -> Traversal Coords
type Traversal a = [Step a]
data Step a = Forward a | Backtrack a deriving (Show, Functor)

marks = L.cycle "abcdefghijklmnopqrstuvyz"

stepValue :: Step a -> a
stepValue (Forward   c) = c
stepValue (Backtrack c) = c

rowByRowTraversal :: TraversalGen
rowByRowTraversal (width, height) =
  [Forward (row, col) | row <- [0..height-1] , col <- dir row [0..width-1]]
  where
    dir row = if even row then id else reverse

zipI :: [a] -> [(Int, a)]
zipI = zip [0..]

randomDeepTraversal :: StdGen -> TraversalGen
randomDeepTraversal rndGen dims@(width, height) =
  reverse $ evalState (tree dims start []) (rndGen, allNodes)
  where
    start = (0, 0)
    allNodes = S.fromList [(r, c) | r <- [0..height-1], c <- [0..width-1]]

tree :: Dimensions -> Coords -> [Step Coords] -> State (StdGen, S.Set Coords) [Step Coords]
tree dims node trav = do
  (rndGen, unvisited) <- get
  if node `S.member` unvisited
    then do
      put (rndGen, S.delete node unvisited)
      if null $ unvisitedNeighbours unvisited
      then return (Backtrack node:trav)
      else do
        (rndGen, unvisited) <- get
        neighs <- shuffle $ unvisitedNeighbours unvisited
        put (rndGen, unvisited)
        foldM visit (Forward node:trav) neighs
    else return trav
  where
    unvisitedNeighbours unvisited = filter (`S.member` unvisited) $ neighbours dims node
    visit trav neighbour = tree dims neighbour trav

shuffle :: [a] -> State (StdGen, (S.Set Coords)) [a]
shuffle as = liftM (V.toList . foldr f v) $ rndIndexes $ length as
  where
    f i acc = let x = acc V.! i; y = acc V.! 0 in set i y (set 0 x acc)
    v = V.fromList as

rndIndexes :: Int -> State (StdGen, (S.Set Coords)) [Int]
rndIndexes n = sequence $ replicate n randomInt
  where
    randomInt = do
      (rndGen, coords) <- get
      let (i, rndGen') = randomR (0, n- 1) rndGen
        in put (rndGen', coords) >> return i

unvisitedNeighbours :: Dimensions -> S.Set Coords -> Coords -> [Coords]
unvisitedNeighbours dims unvisited coords =
  filter (flip S.member unvisited) (neighbours dims coords)

neighbours :: Dimensions -> Coords -> [Coords]
neighbours (w, h) (r, c) = filter onGrid [(r+ro, c+co) | (ro, co) <- offsets]
  where
    onGrid (r, c) = (r >= 0) && (r < h) && (c >= 0) && (c < w)

offsets = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

toString :: Dimensions -> [Step Coords] -> String
toString dims trav = (L.intercalate eol $ list) ++ eol
  where 
    list = toList2 $ markTrav dims (zip trav marks)
    eol = "\n"

markTrav :: Dimensions -> [(Step Coords, Char)] -> Grid Char
markTrav (width, height) = foldr mark (blank width height)
  where
    mark (Backtrack (row, col), char) = set2 row col (toUpper char)
    mark (Forward (row, col), char)   = set2 row col char

toList2 :: V.Vector (V.Vector a) -> [[a]]
toList2 = V.toList . V.map V.toList

set :: Int -> a -> V.Vector a -> V.Vector a
set i e v = v V.// [(i, e)]

set2 :: Int -> Int -> a -> Grid a -> Grid a
set2 row col e grid = set row (set col e (grid V.! row)) grid

blank :: Int -> Int -> V.Vector (V.Vector Char)
blank width height = V.replicate height row
  where
    row = V.replicate width '-'