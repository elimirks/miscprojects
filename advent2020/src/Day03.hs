module Day03 where

import Helper

data TS = TS
  { _count :: Int
  , _x :: Int }

readInput :: IO [String]
readInput = lines <$> readFile "data/day03"

updateTS :: Int -> TS -> String -> TS
updateTS n (TS count x) line = TS (count + count') (x + n)
  where
    -- Scary but hey, gets the job done
    count' = if line !! (x `mod` length line) == '#' then 1 else 0

solveItN :: Int -> [String] -> Int
solveItN n xs = _count $ foldl (updateTS n) (TS 0 0) xs

run03 :: IO ()
run03 = do
  let solvers = (solveItN 1 . dropEven):(solveItN <$> [1, 3, 5, 7])

  solutions <- applyAll solvers <$> readInput
  putStrLn $ show $ foldr (*) 1 solutions
