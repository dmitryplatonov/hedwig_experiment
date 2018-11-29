module Counter (Model, Msg, init, update, view, bound) where

import Prelude
import Data.Lens as L
import Hedwig as H
import Hedwig (button, text, onClick)

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
  text $ show model,
  button [onClick Increment] [text "+"]
]

-- bindComponent v u toMsg lens model =
--   (\msg -> toMsg $ L.over lens $ u msg) <$> v (L.view lens model)

bound :: forall appMsg appModel. ((appModel -> appModel) -> appMsg) -> (L.Lens' appModel Model) -> appModel -> H.Html appMsg
-- bound toMsg lens model = bindComponent view update toMsg lens model
bound toMsg lens model =
  (\msg -> toMsg $ L.over lens $ update msg) <$> view (L.view lens model)
