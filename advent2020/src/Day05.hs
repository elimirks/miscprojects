module Day05 (run05) where

import Control.Applicative
import Data.List
import qualified Data.HashSet as HS

import Helper
import Parser

data Half = Lower | Upper
  deriving (Show)

lowerP :: Parser Half
lowerP = Lower <$ (charP 'L' <|> charP 'F')

upperP :: Parser Half
upperP = Upper <$ (charP 'R' <|> charP 'B')

divisionP :: Parser [Half]
divisionP = many $ lowerP <|> upperP

boardingP :: Parser Integer
boardingP = halfAsInt <$> filterP ((== 10) . length) divisionP

entriesP :: Parser [Integer]
entriesP = sepBy (charP '\n') boardingP <* ws <* eof

readInput :: IO [Integer]
readInput = orElse [] <$> parseFile "data/day05" entriesP

halfAsInt :: [Half] -> Integer
halfAsInt []         = 0
halfAsInt (Lower:xs) = halfAsInt xs
halfAsInt (Upper:xs) = halfAsInt xs + (2 ^ (length xs))

findMySeat :: [Integer] -> Maybe Integer
findMySeat ints = find f [1..1024]
  where
    hashedSeatIds = HS.fromList ints

    f :: Integer -> Bool
    f num = not (HS.member num hashedSeatIds) &&
            HS.member (num - 1) hashedSeatIds &&
            HS.member (num + 1) hashedSeatIds

run05 :: IO ()
run05 = do
  seatIds <- readInput

  putStrLn "Part 5.1:"
  putStrLn $ show $ maximum seatIds

  putStrLn "Part 5.2:"
  putStrLn $ show $ findMySeat seatIds
