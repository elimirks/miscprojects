module Day01 where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

-- Finds the sum of the given list that matches a given target
-- O(n) solution
find2Sum :: Integer -> [Integer] -> Maybe (Integer, Integer)
find2Sum target nums = findSum' nums
  where
    findSum' :: [Integer] -> Maybe (Integer, Integer)
    findSum' []     = Nothing
    findSum' (x:xs) = if HS.member x lookupSet
      then Just (x, target - x)
      else findSum' xs

    lookupSet :: HS.HashSet Integer
    lookupSet = HS.fromList $ (target -) <$> nums

-- O(n^2) solution :( so slow, so sad
find3Sum :: Integer -> [Integer] -> Maybe (Integer, Integer, Integer)
find3Sum target nums = findSum' nums
  where
    findSum' :: [Integer] -> Maybe (Integer, Integer, Integer)
    findSum' []     = Nothing
    findSum' (x:xs) =
      case HM.lookup x lookupMap of
        Just (y, z) -> Just (x, y, z)
        Nothing     -> findSum' xs

    -- Mapping from (target - pair) -> pair
    lookupMap :: HM.HashMap Integer (Integer, Integer)
    lookupMap = HM.fromList $ do
      x <- nums
      y <- nums
      pure (target - x - y, (x, y))

multiplied2Sum :: [Integer] -> Maybe Integer
multiplied2Sum nums = (uncurry (*)) <$> find2Sum 2020 nums

multiplied3Sum :: [Integer] -> Maybe Integer
multiplied3Sum nums = (\(x, y, z) -> x * y * z) <$> find3Sum 2020 nums

readInput :: IO [Integer]
readInput = (read <$>) <$> lines <$> readFile "data/day01"

run1 :: IO ()
run1 = do
  inputNums <- readInput
  putStrLn "2sum:"
  putStrLn $ show $ multiplied2Sum inputNums

  putStrLn "3sum:"
  putStrLn $ show $ multiplied3Sum inputNums
