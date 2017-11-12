module Lib where

import Text.Printf

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Direction = N | E | S | W deriving Eq

instance Show Direction where
  show d = case d of
    N -> "NORTH"
    E -> "EAST"
    S -> "SOUTH"
    W -> "WEST"

right' :: Direction -> Direction
right' d = case d of
  N -> E
  E -> S
  S -> W
  w -> N

left' :: Direction -> Direction
left' d = case d of
  N -> W
  E -> N
  S -> E
  W -> S

move' :: (Int, Int) -> Direction -> (Int, Int)
move' (x, y) d = (x + dx, y + dy) where
  (dx, dy) = case d of
    N -> ( 0,  1)
    E -> ( 1,  0)
    S -> ( 0, -1)
    W -> (-1,  0)

onTheTable' :: Int -> Table -> Bool
onTheTable' x (Table size) = 0 <= x && x < size

onTheTable :: (Int, Int) -> Table -> Bool
onTheTable (x, y) t = all (`onTheTable'` t) [x, y]

data Robot = Robot {
    x :: Int,
    y :: Int,
    direction :: Direction
  }
  deriving Eq

instance Show Robot where
  show (Robot x y d) = printf "%d, %d, %s" x y (show d)

left :: Robot -> Robot
left (Robot x y d) =
  Robot x y (left' d)

right :: Robot -> Robot
right (Robot x y d) =
  Robot x y (right' d)

move :: Robot -> Robot
move (Robot x y d) =
  Robot x' y' d where (x', y') = move' (x, y) d

data Command = Place Int Int Direction | Move | Left | Right | Report

data Table = Table Int

run :: Maybe Robot -> Table -> Command -> (Maybe Robot, Maybe String)
run Nothing table (Place x y d) =
  if onTheTable (x, y) table then (Just $ Robot x y d, Nothing) else (Nothing, Nothing)
run Nothing _ _ = (Nothing, Nothing)
run (Just (Robot x y d)) table command = 
  case command of
    Place x' y' d' -> if onTheTable (x', y') table then (Just $ Robot x' y' d', Nothing) else (Just $ Robot x y d, Nothing)
    Move -> if onTheTable (x', y') table then (Just $ Robot x' y' d, Nothing) else (Just $ Robot x y d, Nothing)
      where (x', y') = move' (x, y) d
    Lib.Left -> (Just $ left (Robot x y d), Nothing)
    Lib.Right-> (Just $ right (Robot x y d), Nothing)
    Report -> (Just $ Robot x y d, Just $ show (Robot x y d))