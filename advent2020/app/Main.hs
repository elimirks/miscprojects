module Main where

import System.Environment
import System.Exit

import Helper

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10

runners :: [IO ()]
runners = [run01, run02, run03, run04, run05, run06, run07, run08, run09, run10]

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
