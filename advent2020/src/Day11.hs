module Day11 (run11) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Tuple
import System.Exit
import qualified Data.Vector as V

import Helper
import Parser

data Grid = Grid
  { _tiles :: (V.Vector Tile)
  , _width :: Int }
  deriving Eq

instance Show Grid where
  show (Grid tiles width) =
      foldl (<>) "" (uncurry showTile <$> V.indexed tiles)
    where
      showTile :: Int -> Tile -> String
      showTile idx tile = show tile <>
        if (idx + 1) `mod` width == 0
          then "\n"
          else ""

data Tile = Floor | Empty | Occupied
  deriving Eq

instance Show Tile where
  show Floor    = "."
  show Empty    = "L"
  show Occupied = "#"

tileP :: Parser Tile
tileP = (Floor    <$ charP '.') <|>
        (Empty    <$ charP 'L') <|>
        (Occupied <$ charP '#')

gridP :: Parser Grid
gridP = do
  (x:xs) <- sepBy (charP '\n') (some tileP) <* ws <* eof
  let width = length x
  let tiles = V.fromList $ join $ x:xs
  pure $ Grid tiles width

readInput :: IO Grid
readInput = do
  grid <- parseFile "data/day11" gridP

  case grid of
    Just grid' -> pure grid'
    Nothing    -> die "Error parsing grid"

countOccupied :: Grid -> Integer
countOccupied = countPredicate (Occupied ==) . _tiles

toCoord :: Grid -> Int -> (Int, Int)
toCoord (Grid _ width) idx = swap $ quotRem idx width

fromCoord :: Grid -> (Int, Int) -> Int
fromCoord (Grid _ width) (x, y) = x + y * width

atCoord :: Grid -> (Int, Int) -> Maybe Tile
atCoord grid@(Grid tiles width) coord@(x, _) =
  -- Only need to check x coords for bounds, V.!? takes care of y bounds
  if x < 0 || x >= width
    then Nothing
    else tiles V.!? fromCoord grid coord

tileInSight :: Grid -> (Int, Int) -> (Int, Int) -> Maybe Tile
tileInSight grid (originX, originY) dirCoord@(dirX, dirY) =
    case atCoord grid nextCoord of
      Just Floor -> tileInSight grid nextCoord dirCoord
      other      -> other
  where
    nextCoord = (originX + dirX, originY + dirY)

surrounding1 :: Grid -> Int -> [Tile]
surrounding1 (Grid tiles width) idx =
    catMaybes $ (tiles V.!?) <$> adjacentIndices
  where
    x :: Int
    x = rem idx width

    leftIndices =
      if x == 0
        then []
        else (idx - 1 +) <$> [0, width, -width]

    rightIndices =
      if x == width - 1
        then []
        else (idx + 1 +) <$> [0, width, -width]

    adjacentIndices =
      [idx - width, idx + width] <> leftIndices <> rightIndices

tick1 :: Grid -> Grid 
tick1 grid@(Grid tiles width) = Grid newTiles width
  where
    newTiles = uncurry tickTile <$> V.indexed tiles

    tickTile :: Int -> Tile -> Tile
    tickTile _ Floor  = Floor
    tickTile idx tile =
        if adjacent == 0 then      Occupied
        else if adjacent >= 4 then Empty
        else                       tile
      where
        adjacent = countPredicate (Occupied ==) $ surrounding1 grid idx

surrounding2 :: Grid -> Int -> [Tile]
surrounding2 grid idx = catMaybes $ (tileInSight grid origin) <$> [
      (-1, -1), (0, -1), (1, -1),
      (-1,  0),          (1,  0),
      (-1,  1), (0,  1), (1,  1)
    ]
  where
    origin = toCoord grid idx

tick2 :: Grid -> Grid 
tick2 grid@(Grid tiles width) = Grid newTiles width
  where
    newTiles = uncurry tickTile <$> V.indexed tiles

    tickTile :: Int -> Tile -> Tile
    tickTile _ Floor  = Floor
    tickTile idx tile =
        if adjacent == 0 then      Occupied
        else if adjacent >= 5 then Empty
        else                       tile
      where
        adjacent = countPredicate (Occupied ==) $ surrounding2 grid idx

run11 :: IO ()
run11 = do
  input <- readInput
  putStrLn "Part 11.1:"
  putStrLn $ show $ countOccupied $ applyUntilNoChange tick1 input
  putStrLn "Part 11.2:"
  putStrLn $ show $ countOccupied $ applyUntilNoChange tick2 input
