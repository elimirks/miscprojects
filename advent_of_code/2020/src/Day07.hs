module Day07 (run07) where

import Control.Applicative
import Data.Char
import Data.List
import qualified Data.HashSet as HS

import Parser
import Helper

type Bag = String

data BagCount = BagCount
  { _count :: Integer
  , _name  :: Bag }
  deriving Show

data Rule = Rule
  { _rname :: Bag
  , _children :: [BagCount] }
  deriving Show

bagP :: Parser Bag
bagP = do
  adjective <- spanP isAlpha
  colour    <- ws *> spanP isAlpha
  _         <- ws *> stringP "bag" *> (optionP $ charP 's')
  pure $ adjective <> " " <> colour

bagCountP :: Parser BagCount
bagCountP = BagCount <$> (intP <* ws) <*> bagP

bagCountsP :: Parser [BagCount]
bagCountsP = notNull (sepBy (charP ',' *> ws) bagCountP)
  <|> [] <$ stringP "no other bags" 

ruleP :: Parser Rule
ruleP = do
  bag <- bagP <* ws <* stringP "contain" <* ws
  Rule bag <$> bagCountsP <* charP '.'

-- Inefficient but simple solution... graphs in Haskell are painful
findAllParents :: [Bag] -> [Rule] -> [Bag]
findAllParents needles rules =
    if length parentBags == 0
      then needles
      else newNeedles <> findAllParents newNeedles nonParentRules
  where
    newNeedles = needles <> parentBags
    parentBags = _rname <$> parentRules

    (parentRules, nonParentRules) = partition ruleContainsNeedle rules

    ruleContainsNeedle :: Rule -> Bool
    ruleContainsNeedle = (0 /=) . length . intersect needles . childBags

    childBags :: Rule -> [Bag]
    childBags (Rule _ children) = _name <$> children

countGoldParents :: [Rule] -> Int
countGoldParents rules = length parentsAndSelf - 1
  where parentsAndSelf = HS.fromList $ findAllParents ["shiny gold"] rules

run07 :: IO ()
run07 = do
  input <- parseFileLines "data/day07" ruleP
  putStrLn "Part 7.1:"
  putStrLn $ show $ countGoldParents input
