module Main where

import System.Environment
import System.Exit

import Helper

import Day01
import Day02
import Day03
import Day04
import Day05

runners :: [IO ()]
runners = [run01, run02, run03, run04, run05]

dieWithUsage :: IO a
dieWithUsage = die "Usage: advent [digit]"

pickRunner :: IO Int
pickRunner = do
  args <- getArgs

  case safeRead <$> args of
    [Just value] -> pure $ fromIntegral $ value - 1
    _            -> dieWithUsage

main :: IO ()
main = do
  num <- pickRunner

  if num >= 0 && num <= length runners
    then runners !! num
    else dieWithUsage
