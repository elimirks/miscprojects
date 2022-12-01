module Helper where

import Data.Char
import System.Exit

import Parser

countPredicate :: Foldable f => (a -> Bool) -> f a -> Integer
countPredicate predicate = foldr f 0
  where
    f it acc | predicate it = acc + 1
             | otherwise    = acc

atIndex :: Integer -> [a] -> Maybe a
atIndex _ []     = Nothing
atIndex 0 (x:_)  = Just x
atIndex n (_:xs) = if n < 0
  then Nothing
  else atIndex (n - 1) xs

dropEven :: [a] -> [a]
dropEven []  = []
dropEven [x] = [x]
dropEven (x:_:xs) = x:dropEven xs

applyAll :: [a -> b] -> a -> [b]
applyAll fs x = ($ x) <$> fs

safeRead :: String -> Maybe Integer
safeRead value = if all isDigit value
  then Just $ read value
  else Nothing

isDigitWithin :: String -> Integer -> Integer -> Bool
isDigitWithin s least most = case safeRead s of
  Just value -> value >= least && value <= most
  _          -> False

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing    = Nothing
filterMaybe f (Just val) =
  if f val then
    Just val
  else
    Nothing

readInputIntegers :: String -> IO [Integer]
readInputIntegers file = (read <$>) <$> lines <$> readFile file

applyUntilNoChange :: Eq a => (a -> a) -> a -> a
applyUntilNoChange f initial = 
    if next /= initial
      then applyUntilNoChange f next
      else initial
  where
    next = f initial

parseFileLines :: String -> Parser a -> IO [a]
parseFileLines path parser = do
  let parser' = sepBy (charP '\n') parser <* ws <* eof
  parsed <- parseFile path parser'

  case parsed of
    Just parsed' -> pure parsed'
    _            -> die $ "Failed to parse " <> path
