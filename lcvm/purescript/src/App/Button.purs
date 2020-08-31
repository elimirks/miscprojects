module App.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Interpreter.Runner (eval)

type State = {
  inputContent :: String,
  result :: String
}

data Action = Increment | Compile | Update String

baseInput :: String
baseInput = """# Taken from https://en.wikipedia.org/wiki/Lambda_calculus
# 0 := λf.λx.x
# 1 := λf.λx.f x
# 2 := λf.λx.f (f x)
# 3 := λf.λx.f (f (f x))
# M N P may be written instead of ((M N) P)

0 := \f.\x.x
1 := \f.\x.(f x)
#succ := \n.\f.\x.(f ((n f) x))
succ := \n.\g.\y.(g ((n g) y))
plus := \m.\n.\f.\x.((m f) ((n f) x))

# Maths go here, should return 2
#((plus (succ 0)) 1)
(succ 0)
"""

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { inputContent: baseInput, result: "" }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
  [
    HH.h1_ [ HH.text "Lambda Calculus" ]
  ,
    HH.textarea
      [ HP.value $ state.inputContent,
        HP.rows 30, HP.cols 80,
        HE.onValueChange $ \s -> Just $ Update s ]
  ,
    HH.br_
  ,
    HH.button [ HE.onClick $ \_ -> Just Compile ] [ HH.text "Run" ]
  ,
    HH.p_ [ HH.text "Result:" ]
  ,
    HH.div_ [ HH.text $ state.result ]
  ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment ->
    H.modify_ \st -> st
  Update input -> do
    H.modify_ \st -> st {
      inputContent = input
    }
  Compile ->
    H.modify_ \st -> st {
      result = eval st.inputContent
    }

