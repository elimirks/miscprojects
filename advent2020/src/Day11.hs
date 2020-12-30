module Day11 (run11) where

import Control.Applicative
import Control.Monad
import Data.Maybe
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

surrounding :: Grid -> Int -> [Tile]
surrounding (Grid tiles width) idx =
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

tick :: Grid -> Grid 
tick grid@(Grid tiles width) = Grid newTiles width
  where
    newTiles = uncurry tickTile <$> V.indexed tiles

    tickTile :: Int -> Tile -> Tile
    tickTile _ Floor  = Floor
    tickTile idx tile =
        if adjacent == 0 then      Occupied
        else if adjacent >= 4 then Empty
        else                       tile
      where
        adjacent = countPredicate (Occupied ==) $ surrounding grid idx

countOccupied :: Grid -> Integer
countOccupied = countPredicate (Occupied ==) . _tiles

tickUntilNoChange :: Grid -> Grid
tickUntilNoChange grid =
    if nextGrid /= grid
      then tickUntilNoChange nextGrid
      else grid
  where
    nextGrid = tick grid

run11 :: IO ()
run11 = do
  input <- readInput
  putStrLn "Part 11.1:"
  putStrLn $ show $ countOccupied $ tickUntilNoChange input
