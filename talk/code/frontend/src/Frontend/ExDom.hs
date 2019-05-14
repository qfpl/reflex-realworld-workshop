{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.ExDom where

import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)

exDom :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
exDom = do
  buttonClickE :: Event t () <- button "Click me"
  clickCountDyn :: Dynamic t Int <- count buttonClickE
  text " Clicked: "
  display clickCountDyn
  text " times!"
