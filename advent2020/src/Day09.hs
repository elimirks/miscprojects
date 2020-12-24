module Day09 (run09) where

import Data.Maybe
import Control.Applicative

import Helper
import Parser

-- Good old brute force solutions... yikes bikes

solve1 :: [Integer] -> Maybe Integer
solve1 []          = Nothing
solve1 allX@(_:xs) = filterMaybe (not . flip elem combos) target <|> solve1 xs
  where
    preamble = take 25 allX
    combos = [a + b | a <- preamble, b <- preamble]

    target :: Maybe Integer
    target = listToMaybe $ drop 25 allX

solve2 :: [Integer] -> Integer -> Maybe Integer
solve2 [] _ = Nothing
solve2 [_] _ = Nothing
solve2 l@(_:xs) target = solve2' (reverse l) <|> solve2 xs target
  where
    -- Reversed list as the arg...
    solve2' :: [Integer] -> Maybe Integer
    solve2' l' | length l' < 2 = Nothing
    solve2' l' =
      if sum l' == target then
        -- trace (show $ head l' + last l') trace (show $ l') $ trace (show $ sum l')
        Just $ head l' + last l'
      else
        solve2' $ tail l'

numsP :: Parser [Integer]
numsP = sepBy (charP '\n') intP <* ws <* eof

readInput :: IO [Integer]
readInput = fromMaybe [] <$> parseFile "data/day09" numsP

run09 :: IO ()
run09 = do
  input <- readInput
  putStrLn "Part 9.1:"
  let solution1 = solve1 input
  putStrLn $ show $ solution1
  -- For some reason this solution doesn't work on the site...
  -- even though the range sums to the answer to part 1
  putStrLn $ show $ solution1 >>= solve2 input
