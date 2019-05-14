{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.ExFoldDyn where

import Reflex.Dom.Core
import Data.Monoid (Endo(..),appEndo)
import Control.Monad.Fix (MonadFix)

exFoldDyn :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
exFoldDyn = do
  buttonClickE :: Event t () <- button "Click me"
  let addE = Endo (+1) <$ buttonClickE
  clickCountDyn :: Dynamic t Int <- foldDyn appEndo 0 addE
  text " Clicked: "
  display clickCountDyn
  text " times!"
