module Main where

newtype StateT s a = StateT { runState :: s -> (s, a) }

-- Proof that StateT is a Functor
instance Functor (StateT s) where
  fmap f state = StateT $ \s ->
    let (_, value) = runState state s
    in (s, f value)

-- Proof that StateT is Applicative
instance Applicative (StateT s) where
  pure val = StateT $ \s -> (s, val)
  stateF <*> stateVal = StateT $ \s ->
    let
      (_, f) = runState stateF s
      (_, val) = runState stateVal s
    in
      (s, f val)

-- Pentultimate proof that StateT is monadic
instance Monad (StateT s) where
  -- StateT s a -> (a -> StateT s b) -> StateT s b
  stateVal >>= f = StateT $ \s ->
    let (s', value) = runState stateVal s
    in runState (f value) s'

get :: StateT s s
get = StateT $ \s -> (s, s)

put :: s -> StateT s ()
put state = StateT $ \_ -> (state, ())

sel2 :: (a, b) -> b
sel2 (_, value) = value

evalState :: s -> StateT s a -> a
evalState state initial = sel2 $ runState initial state

type CounterState a = StateT Int a

-- Epic Haskell Programmer
tick :: CounterState ()
tick = (+1) <$> get >>= put

useTheStateForSomething :: CounterState Int
useTheStateForSomething = do
  tick
  tick
  tick
  get

main :: IO ()
main = do
  putStrLn $ show $ evalState 0 useTheStateForSomething
