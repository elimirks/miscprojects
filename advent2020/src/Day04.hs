module Day04 (run04) where

import Control.Applicative
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.HashMap.Lazy as HM

import Parser
import Helper

data Entry = Entry (HM.HashMap String String)
  deriving (Show)

componentP :: Parser String
componentP = spanP predicate
  where predicate c = c /= ' ' && c /= ':' && c /= '\n'

metaP :: Parser (String, String)
metaP = do
  key   <- componentP <* charP ':'
  value <- componentP
  pure $ (key, value)

entryP :: Parser Entry
entryP = Entry . HM.fromList <$> sepBy metaDelimP metaP
  where metaDelimP = () <$ (charP '\n' <|> charP ' ')

entriesP :: Parser [Entry]
entriesP = sepBy (stringP "\n\n") entryP <* ws <* eof

readInput :: IO [Entry]
readInput = fromMaybe [] <$> parseFile "data/day04" entriesP

isPassport1 :: Entry -> Bool
isPassport1 (Entry fields) = keys == expected1 || keys == expected2
  where
    keys :: [String]
    keys = sort $ HM.keys fields

    expected1 = ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]
    expected2 = ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]

heightParser :: Parser (Integer, String)
heightParser = do
  value <- intP
  unit  <- stringP "cm" <|> stringP "in"
  (value, unit) <$ eof

isValidHeight :: String -> Bool
isValidHeight value = case evalParser heightParser value of
  Just (parsed, "cm") -> parsed >= 150 && parsed <= 193
  Just (parsed, "in") -> parsed >= 59 && parsed <= 76
  _                   -> False

isValidColor :: String -> Bool
isValidColor ('#':xs) | length xs == 6 = all isHex xs
  where
    isHex :: Char -> Bool
    isHex c = isDigit c || elem c "abcdef"
isValidColor _ = False

isFieldValid :: String -> String -> Bool
isFieldValid "cid" _     = True
isFieldValid "ecl" value =
  elem value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isFieldValid "pid" value = length value == 9 && all isDigit value
isFieldValid "byr" value = isDigitWithin value 1920 2002
isFieldValid "iyr" value = isDigitWithin value 2010 2020
isFieldValid "eyr" value = isDigitWithin value 2020 2030
isFieldValid "hgt" value = isValidHeight value
isFieldValid "hcl" value = isValidColor value
isFieldValid _ _         = False

isPassport2 :: Entry -> Bool
isPassport2 entry@(Entry fields) =
  isPassport1 entry && all (uncurry isFieldValid) (HM.toList fields)

run04 :: IO ()
run04 = do
  input <- readInput
  putStrLn "Part 1:"
  putStrLn $ show $ length (filter isPassport1 input)
  putStrLn "Part 2:"
  putStrLn $ show $ length (filter isPassport2 input)
