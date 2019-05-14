{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Frontend where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Common.Route

import Frontend.ExDom (exDom)
import Frontend.ExFoldDyn (exFoldDyn)
import Frontend.ExMergeEvents (exMergeEvents)
import Frontend.ExEventWriter (exEventWriter)
import Frontend.ExReader (exReader)
import Frontend.ExRoutes (exRoutes)
import Frontend.ExWorkflow (exWorkflow)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "Obelisk Minimal Example"
    el "style" $ text "body { font-size: 1.5em; }"
  , _frontend_body = do
    subRoute_ $ \case
      FrontendRoute_Main -> blank
      FrontendRoute_ExDom -> exDom
      FrontendRoute_ExFoldDyn -> exFoldDyn
      FrontendRoute_ExMergeEvents -> exMergeEvents
      FrontendRoute_ExEventWriter -> exEventWriter
      FrontendRoute_ExReader -> exReader
      FrontendRoute_ExRoutes -> exRoutes
      FrontendRoute_ExWorkflow -> exWorkflow
  }
