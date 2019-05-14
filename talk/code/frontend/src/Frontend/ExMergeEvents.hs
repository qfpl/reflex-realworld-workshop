{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.ExMergeEvents where

import Reflex.Dom.Core
import Data.Monoid (Endo(..),appEndo)
import Control.Monad.Fix (MonadFix)

exMergeEvents :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
exMergeEvents = do
  addClickE   <- button "Click me"
  resetClickE <- button "Reset me"

  let addE     = Endo (+1)      <$ addClickE
  let resetE   = Endo (const 0) <$ resetClickE 
  let updatesE = addE <> resetE

  clickCountDyn <- foldDyn appEndo 0 updatesE
  text " Clicked: "
  display clickCountDyn
  text " times!"
