{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecursiveDo                                                                             #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                             #-}
module Frontend.Login where

import Control.Lens
import Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Data.Foldable          (traverse_)
import           Data.Functor           (void)
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute, routeLink)

import           Common.Conduit.Api.Namespace         (Namespace (Namespace), unNamespace)
import           Common.Conduit.Api.Users.Credentials (Credentials (Credentials))
import           Common.Route                         (FrontendRoute (..))
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass)

login
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , EventWriter t (NonEmpty e) m
     , MonadHold t m
     , MonadFix m
     , AsFrontendEvent e
     , HasLoggedInAccount s
     , HasFrontendState t s m
     )
  => m ()
login = elClass "div" "auth-page" $ do
  elClass "div" "container-page" $ do
    elClass "div" "row" $ do
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ mdo
        elClass "h1" "text-xs-center" $ text "Sign in"
