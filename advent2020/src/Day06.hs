module Day06 (run06) where

import Data.Maybe
import Data.List
import Data.Char
import Control.Monad
import qualified Data.HashSet as HS

import Parser

data Group = Group [String]
  deriving Show

lineP :: Parser String
lineP = notNull $ spanP isAlpha

groupP :: Parser Group
groupP = Group <$> (notNull $ sepBy (charP '\n') lineP)

entriesP :: Parser [Group]
entriesP = sepBy (stringP "\n\n") groupP <* ws <* eof

readInput :: IO [Group]
readInput = fromMaybe [] <$> parseFile "data/day06" entriesP

anyAnswerCount :: Group -> Int
anyAnswerCount (Group answers) = HS.size $ HS.fromList $ join answers

allAnswerCount :: Group -> Int
allAnswerCount (Group answers) =
  let (x:xs) = HS.fromList <$> answers
  in length $ foldl' HS.intersection x xs

run06 :: IO ()
run06 = do
  input <- readInput
  putStrLn "Part 6.1:"
  putStrLn $ show $ sum $ anyAnswerCount <$> input

  putStrLn "Part 6.2:"
  putStrLn $ show $ sum $ allAnswerCount <$> input
