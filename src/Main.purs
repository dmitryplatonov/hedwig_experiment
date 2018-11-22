module Main where

import Prelude

import Effect (Effect)
import Hedwig as H
import Hedwig (button, text, onClick, (:>))

type Model = { counter:: Int }

init :: Model
init = { counter: 0 }

data Msg = Increment | Decrement

update :: Model -> Msg -> Model
update model = case _ of
  Increment -> model { counter = model.counter + 1 }
  Decrement -> model { counter = model.counter - 1 }

view :: Model -> H.Html Msg
view model = counter model.counter

counter :: Int -> H.Html Msg
counter model = H.main [H.id "main"] [
  button [onClick Decrement] [text "-"],
  text (show model),
  button [onClick Increment] [text "+"]
]

main :: Effect Unit
main = do
  H.mount "main" {
    init: init :> [],
    update: \msg model -> update msg model :> [],
    view
}
