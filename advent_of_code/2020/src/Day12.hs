module Day12 (run12) where

import Control.Applicative
import Control.Monad
import Data.List

import Parser
import Helper

data TurnAmount = Clockwise | Anticlockwise | Backward
data Cardinal = North | South | East | West
data Action = Turn TurnAmount | Forward Integer | Move Cardinal Integer

data Ship = Ship
  { _direction :: Cardinal
  , _x :: Integer
  , _y :: Integer }

turnP :: Parser Action
turnP = Turn <$> (
    (Anticlockwise <$ stringP "L90")  <|>
    (Anticlockwise <$ stringP "R270") <|>
    (Clockwise     <$ stringP "R90")  <|>
    (Clockwise     <$ stringP "L270") <|>
    (Backward      <$ stringP "R180") <|>
    (Backward      <$ stringP "L180")
  )

forwardP :: Parser Action
forwardP = Forward <$> (charP 'F' *> intP)

cardinalP :: Parser Cardinal
cardinalP =
  North <$ charP 'N' <|>
  South <$ charP 'S' <|>
  East  <$ charP 'E' <|>
  West  <$ charP 'W'

moveP :: Parser Action
moveP = liftM2 Move cardinalP intP

actionP :: Parser Action
actionP = turnP <|> forwardP <|> moveP

turnCardinal :: Cardinal -> TurnAmount -> Cardinal
turnCardinal North Clockwise     = East
turnCardinal North Anticlockwise = West
turnCardinal North Backward      = South
turnCardinal South Clockwise     = West
turnCardinal South Anticlockwise = East
turnCardinal South Backward      = North
turnCardinal East Clockwise      = South
turnCardinal East Anticlockwise  = North
turnCardinal East Backward       = West
turnCardinal West Clockwise      = North
turnCardinal West Anticlockwise  = South
turnCardinal West Backward       = East

moveShip :: Ship -> Cardinal -> Integer -> Ship
moveShip (Ship direction x y) North amount = Ship direction x (y - amount)
moveShip (Ship direction x y) South amount = Ship direction x (y + amount)
moveShip (Ship direction x y) East amount  = Ship direction (x + amount) y
moveShip (Ship direction x y) West amount  = Ship direction (x - amount) y

applyAction :: Ship -> Action -> Ship
applyAction (Ship direction x y) (Turn amount) =
  Ship (turnCardinal direction amount) x y
applyAction ship (Forward amount) =
  moveShip ship (_direction ship) amount
applyAction ship (Move cardinal amount) =
  moveShip ship cardinal amount

manhattan :: (Integer, Integer) -> Integer
manhattan (x, y) = (abs x) + (abs y)

solve1 :: [Action] -> Integer
solve1 actions = manhattan (_x finalShip, _y finalShip)
  where
    finalShip = foldl' applyAction (Ship East 0 0) actions

run12 :: IO ()
run12 = do
  input <- parseFileLines "data/day12" actionP
  putStrLn "Part 12.1:"
  putStrLn $ show $ solve1 input
