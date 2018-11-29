module Utils (Update, View, bindComponent) where

import Prelude
import Hedwig as H
import Data.Lens as L

type Update msg model = msg -> model -> model
type View msg model = model -> H.Html msg

bindComponent:: forall msg model appMsg appModel. View msg model -> Update msg model -> ((appModel -> appModel) -> appMsg) -> L.Lens' appModel model -> View appMsg appModel
bindComponent v u toMsg lens model =
  (\msg -> toMsg $ L.over lens $ u msg) <$> v (L.view lens model)
