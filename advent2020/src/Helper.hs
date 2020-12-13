module Helper where

import Data.Char

countPredicate :: (a -> Bool) -> [a] -> Integer
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

orElse :: a -> Maybe a -> a
orElse _ (Just x) = x
orElse y _        = y

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
