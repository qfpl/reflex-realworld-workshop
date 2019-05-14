{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Frontend.ExWorkflow where

import Reflex.Dom.Core
import Data.Text (Text)
import Data.Functor (void)
import Control.Monad.Fix (MonadFix)

exWorkflow :: ( DomBuilder t m, MonadHold t m , MonadFix m ) => m ()
exWorkflow = void $ workflow page1

page1, page2, page3 :: (DomBuilder t m) => Workflow t m Text
page1 = Workflow . el "div" $ do
  el "div" $ text "This is page 1"
  pg2 <- button "Switch to page 2"
  return ("Page 1", page2 <$ pg2)

page2 = Workflow . el "div" $ do
  el "div" $ text "This is page 2"
  pg3 <- button "Switch to page 3"
  pg1 <- button "No wait, I want to go back to page 1"
  return ("Page 2", leftmost [page3 <$ pg3, page1 <$ pg1])

page3 = Workflow . el "div" $ do
  el "div" $ text "You have arrived on page 3"
  pg1 <- button "Start over"
  return ("Page 3", page1 <$ pg1)
