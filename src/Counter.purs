module Counter (Model, Msg, init, update, view, bound) where

import Prelude
import Data.Lens as L
import Hedwig as H
import Hedwig (button, text, onClick, (:>))

type Model = Int
data Msg = Increment | Decrement | Reset

init :: Model
init = 0

update :: Msg -> Model -> Model
update msg model = case msg of
  Increment -> model + 1
  Decrement -> model - 1
  Reset -> 0

view :: Model -> H.Html Msg
view model = H.div [] [
  button [onClick Decrement] [text "-"],
  text (show model),
  button [onClick Increment] [text "+"]
]

bound :: forall msg appModel. ((appModel -> appModel) -> msg) -> (L.Lens' appModel Model) -> appModel -> H.Html msg
bound toMsg lens model =
  (\msg -> toMsg (L.over lens (update msg))) <$> view (L.view lens model)
