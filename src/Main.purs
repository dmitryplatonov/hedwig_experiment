module Main where
import Prelude

import Effect (Effect)
import Hedwig as H
import Hedwig (button, text, onClick, (:>))
import Data.Lens as L
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

type CounterModel = Int
type Model = { counter1:: CounterModel, counter2:: CounterModel }
type ModelUpdater = Model -> Model
data Msg = Msg ModelUpdater

init :: Model
init = { counter1: 0, counter2: 0 }

data CounterMsg = Increment | Decrement | Reset

counterUpdate :: CounterMsg -> CounterModel -> CounterModel
counterUpdate msg model = case msg of
  Increment -> model + 1
  Decrement -> model - 1
  Reset -> 0

counter1Lens :: L.Lens' Model CounterModel
counter1Lens = prop (SProxy:: SProxy "counter1")

counter2Lens :: L.Lens' Model CounterModel
counter2Lens = prop (SProxy:: SProxy "counter2")

update :: Model -> Msg -> Model
update model = case _ of
  Msg transformer -> transformer model

view :: Model -> H.Html Msg
view model = H.main [] [
  (\msg -> Msg (L.over counter1Lens (counterUpdate msg))) <$> counter (L.view counter1Lens model),
  (\msg -> Msg (L.over counter2Lens (counterUpdate msg))) <$> counter (L.view counter2Lens model)
]

counter :: CounterModel -> H.Html CounterMsg
counter model = H.div [] [
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
