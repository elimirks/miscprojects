module Day08 (run08) where

import Control.Applicative
import Data.Maybe
import qualified Data.HashSet as HS

import Parser

data Instruction = Nop | Acc Int | Jmp Int
  deriving Show

data VM = VM
  { _pc :: Int
  , _accumulator :: Int
  , _pcHistory :: HS.HashSet Int
  , _program :: [Instruction] }

shouldHalt :: VM -> Bool
shouldHalt (VM pc _ history prog) =
  pc < 0 || pc >= length prog || HS.member pc history

-- Visions of Lens ...
jumpVM :: Int -> VM -> VM
jumpVM pcDelta (VM pc acc history prog) = VM pc' acc history' prog
  where
    pc'      = pc + pcDelta
    history' = HS.insert pc history

accVM :: Int -> VM -> VM
accVM accDelta (VM pc acc history prog) = VM pc (acc + accDelta) history prog

runInstruction :: Instruction -> VM -> VM
runInstruction Nop vm       = jumpVM 1 vm
runInstruction (Acc arg) vm = accVM arg . jumpVM 1 $ vm
runInstruction (Jmp arg) vm = jumpVM arg vm

currentInstruction :: VM -> Maybe Instruction
currentInstruction (VM pc _ _ prog) =
  if pc >= 0 && pc < length prog
    then Just $ prog !! pc
    else Nothing

tickVM :: VM -> Maybe VM
tickVM vm = flip runInstruction vm <$> currentInstruction vm

mkVM :: [Instruction] -> VM
mkVM = VM 0 0 HS.empty

runVM :: VM -> VM
runVM vm =
  if shouldHalt vm
    then vm
    else case tickVM vm of
      Just nextVM -> runVM nextVM
      _           -> vm

nopP :: Parser Instruction
nopP = Nop <$ stringP "nop" <* ws <* signedP

accP :: Parser Instruction
accP = Acc . fromIntegral <$> (stringP "acc" *> ws *> signedP)

jmpP :: Parser Instruction
jmpP = Jmp . fromIntegral <$> (stringP "jmp" *> ws *> signedP)

instructionP :: Parser Instruction
instructionP = nopP <|> accP <|> jmpP

instructionsP :: Parser [Instruction]
instructionsP = sepBy (charP '\n') instructionP <* ws <* eof

readInput :: IO [Instruction]
readInput = fromMaybe [] <$> parseFile "data/day08" instructionsP

run08 :: IO ()
run08 = do
  putStrLn "Part 8.1:"
  input <- readInput
  putStrLn $ show $ _accumulator $ runVM . mkVM $ input
