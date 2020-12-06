module Helper where

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
