module Day02 (run02) where

import Data.Maybe

import Helper
import Parser

data Policy = Policy
  { _letter :: Char,
    _firstNum :: Integer,
    _secondNum :: Integer }

data Entry = Entry
  { _policy :: Policy,
    _password :: String }

rangeP :: Parser (Integer, Integer)
rangeP = (,) <$> (intP <* charP '-') <*> intP

policyP :: Parser Policy
policyP = do
  (minC, maxC) <- rangeP <* ws
  letter       <- anyP
  pure $ Policy letter minC maxC

entryP :: Parser Entry
entryP = do
  policy   <- policyP <* charP ':' <* ws
  password <- spanP (/= '\n')
  pure $ Entry policy password

allEntriesP :: Parser [Entry]
allEntriesP = sepBy (charP '\n') entryP

readInput :: IO [Entry]
readInput = fromMaybe [] <$> parseFile "data/day02" allEntriesP

isValidA :: Entry -> Bool
isValidA (Entry (Policy letter minCount maxCount) password) =
    letterCount >= minCount && letterCount <= maxCount
  where
    letterCount = countPredicate (== letter) password

countValidA :: [Entry] -> Integer
countValidA entries = countPredicate isValidA entries

isValidB :: Entry -> Bool
isValidB (Entry (Policy letter posA posB) password) =
    (countPredicate (== letter) chars) == 1
  where
    maybeA = atIndex (posA - 1) password
    maybeB = atIndex (posB - 1) password

    chars = catMaybes [maybeA, maybeB] 

countValidB :: [Entry] -> Integer
countValidB entries = countPredicate isValidB entries

run02 :: IO ()
run02 = do
  input <- readInput

  putStrLn "Part 2.1:"
  putStrLn $ show $ countValidA input

  putStrLn "Part 3.1:"
  putStrLn $ show $ countValidB input
