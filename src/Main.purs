module Main where
import Prelude

import Effect (Effect)
import Hedwig as H
import Hedwig (button, text, onClick, (:>))
import Data.Lens as L
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

type CounterModel = Int
data CounterMsg = Increment | Decrement | Reset

counterUpdate :: CounterMsg -> CounterModel -> CounterModel
counterUpdate msg model = case msg of
  Increment -> model + 1
  Decrement -> model - 1
  Reset -> 0

counter :: CounterModel -> H.Html CounterMsg
counter model = H.div [] [
  button [onClick Decrement] [text "-"],
  text (show model),
  button [onClick Increment] [text "+"]
]

boundCounter :: (L.Lens' Model CounterModel) -> Model -> H.Html Msg
boundCounter lens model =
  (\msg -> Msg (L.over lens (counterUpdate msg))) <$> counter (L.view lens model)
type Model = { counter1:: CounterModel, counter2:: CounterModel }
type ModelUpdater = Model -> Model
data Msg = Msg ModelUpdater

init :: Model
init = { counter1: 0, counter2: 0 }

counter1Lens :: L.Lens' Model CounterModel
counter1Lens = prop (SProxy:: SProxy "counter1")

counter2Lens :: L.Lens' Model CounterModel
counter2Lens = prop (SProxy:: SProxy "counter2")

update :: Model -> Msg -> Model
update model = case _ of
  Msg transformer -> transformer model

view :: Model -> H.Html Msg
view model = H.main [] $ (flip ($) model) <$> [
  boundCounter counter1Lens,
  boundCounter counter2Lens
]

main :: Effect Unit
main = do
  H.mount "main" {
    init: init :> [],
    update: \msg model -> update msg model :> [],
    view
}
