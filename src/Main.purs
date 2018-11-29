module Main where
import Prelude

import Effect (Effect)
import Data.Lens as L
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Data.Profunctor.Strong (class Strong)
import Hedwig as H
import Hedwig ((:>))

import Counter as Counter

type Model = { counter1:: Counter.Model, counter2:: Counter.Model }
type ModelUpdater = Model -> Model
data Msg = Msg ModelUpdater

init :: Model
init = { counter1: Counter.init, counter2: Counter.init }

counter1Lens :: L.Lens' Model Counter.Model
counter1Lens = prop (SProxy:: SProxy "counter1")

counter2Lens :: L.Lens' Model Counter.Model
counter2Lens = prop (SProxy:: SProxy "counter2")

update :: Model -> Msg -> Model
update model = case _ of
  Msg transformer -> transformer model

--counter:: (L.Lens' Model Counter.Model) -> Model -> H.Html Msg
--counter:: (forall p. Strong p => p Int Int -> p { counter1 :: Int, counter2 :: Int } { counter1 :: Int , counter2 :: Int }) -> { counter1 :: Int, counter2 :: Int } -> H.Html Msg
counter = Counter.bound Msg

view :: Model -> H.Html Msg
view model = H.main [] $ ((#) model) <$> [
  counter counter1Lens,
  counter counter2Lens
]

main :: Effect Unit
main = do
  H.mount "main" {
    init: init :> [],
    update: \msg model -> update msg model :> [],
    view
  }
