{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications                                                   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                             #-}

module Frontend.Settings where

import Control.Lens
import Reflex.Dom.Core

import           Control.Monad          (mfilter)
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as Text
import           Obelisk.Route.Frontend (pattern (:/), R, SetRoute, setRoute)

import           Common.Conduit.Api.Namespace    (Namespace (Namespace), unNamespace)
import qualified Common.Conduit.Api.User.Account as Account
import           Common.Conduit.Api.User.Update  (UpdateUser (UpdateUser))
import           Common.Route                    (FrontendRoute (..), Username (..))
import qualified Frontend.Conduit.Client         as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                  (buttonClass)

settings
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , EventWriter t (NonEmpty FrontendEvent) m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
settings = elClass "div" "settings-page" $ do
  elClass "div" "container page" $
    el "h1" $ text "TODO"
