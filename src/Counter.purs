module Counter (Model, Msg, init, update, view, bound) where

import Prelude
import Data.Lens as L
import Hedwig as H
import Hedwig (button, text, onClick)
import Utils as Utils
import Utils (Update, View, bindComponent)

type Model = Int
data Msg = Increment | Decrement | Reset

init :: Model
init = 0

update :: Update Msg Model
update msg model = case msg of
  Increment -> model + 1
  Decrement -> model - 1
  Reset -> 0

view :: View Msg Model
view model = H.div [] [
  button [onClick Decrement] [text "-"],
  text $ show model,
  button [onClick Increment] [text "+"]
]

bound :: forall appMsg appModel. ((appModel -> appModel) -> appMsg) -> (L.Lens' appModel Model) -> View appMsg appModel
bound toMsg lens = bindComponent view update toMsg lens
