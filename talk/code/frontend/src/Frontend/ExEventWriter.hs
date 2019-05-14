{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.ExEventWriter where

import Reflex.Dom.Core
import Data.Monoid (Endo(..),appEndo)
import Control.Monad.Fix (MonadFix)

exEventWriter :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
exEventWriter = do
  (_,updatesE) <- runEventWriterT $ do
    addButton
    resetButton

  clickCountDyn <- foldDyn appEndo 0 updatesE
  text " Clicked: "
  display clickCountDyn
  text " times!"

addButton :: (DomBuilder t m, EventWriter t (Endo Int) m) => m ()
addButton = do
  addClickE   <- button "Click me"
  tellEvent $ Endo (+1) <$ addClickE 

resetButton :: (DomBuilder t m, EventWriter t (Endo Int) m) => m ()
resetButton = do
  resetClickE <- button "Reset me"
  tellEvent $ Endo (const 0) <$ resetClickE 
