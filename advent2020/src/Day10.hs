module Day10 (run10) where

import Data.List

import Helper

computeDiffs :: [Integer] -> [Integer]
computeDiffs xs = uncurry (-) <$> zip xs (0:xs)

solve1 :: [Integer] -> Integer
solve1 input = ones * threes
  where
    diff   = computeDiffs input
    ones   = countPredicate (1 ==) diff
    threes = countPredicate (3 ==) diff + 1 -- +1 for the built in adapter

run10 :: IO ()
run10 = do
  input <- sort <$> readInputIntegers "data/day10"
  putStrLn "Part 10.1:"
  putStrLn $ show $ solve1 input

  putStrLn "Part 10.2:"
  putStrLn $ "Target: " <> show (3 + foldl' max 0 input)
