module Robot where

import System.Exit (exitSuccess)
import Control.Monad (forever)

parseCommand :: String -> Maybe Command
parseCommand s 
  | s == "MOVE"          = Just Move
  | s == "LEFT"          = Just TurnLeft
  | s == "RIGHT"         = Just TurnRight
  | s == "REPORT"        = Just Report
  | take 6 s == "PLACE " = parsePlace s
  | otherwise            = Nothing

parsePlace :: String -> Maybe Command
parsePlace s 
  | length parts /= 3 = Nothing
  | otherwise = Just $ Place (read (parts !! 0)) (read (parts !! 1)) (parseDirection (parts !! 2))
  where parts = splitOnCommas $ drop 6 s

loop :: Maybe Robot -> Table -> IO ()
loop robot table = forever $ do
  line <- getLine
  checkForExit line
  case parseCommand line of
    Just c  -> case run robot table c of
                (r, Just s) -> do
                  putStrLn s
                  loop r table
                (r, _)      ->
                  loop r table
    Nothing -> putStrLn "Failed to parse command, enter nothing to terminate"

checkForExit :: String -> IO ()
checkForExit s = if s == "" then
  do exitSuccess
  else return ()

splitOnCommas :: String -> [String]
splitOnCommas s = case dropWhile (== ',') s of
  "" -> []
  s' -> w : splitOnCommas s''
        where (w, s'') = break (== ',') s'

data Direction = N | E | S | W deriving Eq

parseDirection :: String -> Direction
parseDirection s = case s of
  "NORTH" -> N
  "EAST"  -> E
  "SOUTH" -> S
  "WEST"  -> W

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
  show (Robot x y d) = (show x) ++ ", " ++ (show y) ++ ", " ++ (show d)

left :: Robot -> Robot
left (Robot x y d) =
  Robot x y (left' d)

right :: Robot -> Robot
right (Robot x y d) =
  Robot x y (right' d)

move :: Robot -> Robot
move (Robot x y d) =
  Robot x' y' d where (x', y') = move' (x, y) d

data Command = Place Int Int Direction | Move | TurnLeft | TurnRight | Report deriving (Eq, Show)

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
    TurnLeft -> (Just $ left (Robot x y d), Nothing)
    TurnRight-> (Just $ right (Robot x y d), Nothing)
    Report -> (Just $ Robot x y d, Just $ show (Robot x y d))