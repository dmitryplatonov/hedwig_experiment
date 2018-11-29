module Main where
import Prelude

import Effect (Effect)
import Data.Lens as L
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Hedwig as H
import Hedwig ((:>))

import Utils (View, Update)
import Counter as Counter

type Model = { counter1:: Counter.Model, counter2:: Counter.Model }
type ModelUpdater = Model -> Model
data Msg = Msg ModelUpdater

init :: Model
init = { counter1: Counter.init, counter2: Counter.init }

type ComponentLens model = L.Lens' Model model

counter1Lens :: ComponentLens Counter.Model
counter1Lens = prop $ SProxy:: SProxy "counter1"

counter2Lens :: ComponentLens Counter.Model
counter2Lens = prop $ SProxy:: SProxy "counter2"

update :: Update Msg Model
update msg model = case msg of
  Msg transformer -> transformer model

counter:: (ComponentLens Counter.Model) -> View Msg Model
counter lens = Counter.bound Msg lens

view :: View Msg Model
view model = H.main [] $ (#) model <$> [
  counter counter1Lens,
  counter counter2Lens
]

main :: Effect Unit
main = do
  H.mount "main" {
    init: init :> [],
    update: \model msg -> update msg model :> [],
    view
  }
