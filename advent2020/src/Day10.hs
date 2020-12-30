module Day10 (run10) where

import Data.List

import Helper

computeDiffs :: [Integer] -> [Integer]
computeDiffs xs = uncurry (-) <$> zip xs (0:xs)

solve1 :: [Integer] -> Integer
solve1 input = ones * threes
  where
    diff   = computeDiffs $ sort input
    ones   = countPredicate (1 ==) diff
    threes = countPredicate (3 ==) diff + 1 -- +1 for the built in adapter

solve2 :: [Integer] -> Integer
solve2 input = snd $ head $ dyn
  where
    sorted = sort input
    xs     = sorted <> [3 + last sorted]
    dyn    = foldl' nextRes [(0, 1)] xs -- DP list

    nextRes :: [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
    nextRes previous new = (new, newDyn):previous
      where 
        satisfied = takeWhile ((3 >=) . (new -) . fst) previous
        newDyn    = sum $ snd <$> satisfied

run10 :: IO ()
run10 = do
  input <- readInputIntegers "data/day10"
  putStrLn "Part 10.1:"
  putStrLn $ show $ solve1 input

  putStrLn "Part 10.2:"
  putStrLn $ show $ solve2 input
