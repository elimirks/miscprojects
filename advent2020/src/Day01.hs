module Day01 (run01) where

import Control.Applicative
import Data.List
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Helper

target :: Integer
target = 2020

-- Finds the sum of the given list that matches the target
-- O(n) solution
multiplied2Sum :: [Integer] -> Maybe Integer
multiplied2Sum nums =
    (\x -> x * (target - x)) <$> find (flip HS.member lookupSet) nums
  where
    lookupSet :: HS.HashSet Integer
    lookupSet = HS.fromList $ (target -) <$> nums

-- O(n^2) solution :( so slow, so sad
multiplied3Sum :: [Integer] -> Maybe Integer
multiplied3Sum nums = findSum' nums
  where
    findSum' :: [Integer] -> Maybe Integer
    findSum' []     = Nothing
    findSum' (x:xs) = (x *) <$> HM.lookup x lookupMap <|> findSum' xs

    -- Mapping from (target - pair) -> pair multiplication
    lookupMap :: HM.HashMap Integer Integer
    lookupMap = HM.fromList [(target - x - y, x * y) | x <- nums, y <- nums]

run01 :: IO ()
run01 = do
  input <- readInputIntegers "data/day01"
  putStrLn "Part 1.1:"
  putStrLn $ show $ multiplied2Sum input

  putStrLn "Part 1.2:"
  putStrLn $ show $ multiplied3Sum input
