{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                             #-}
module Frontend.Register where

import Control.Lens
import Reflex.Dom.Core


import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute, routeLink)
import           Servant.Common.Req     (reqSuccess)

import           Common.Conduit.Api.Namespace        (Namespace (Namespace), unNamespace)
import           Common.Conduit.Api.Users.Registrant (Registrant (Registrant))
import           Common.Route                        (FrontendRoute (..))
import qualified Frontend.Conduit.Client             as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                      (buttonClass)


register
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , EventWriter t (NonEmpty e) m
     , AsFrontendEvent e
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
register = noUserWidget $ elClass "div" "auth-page" $ do
  elClass "div" "container-page" $
    elClass "div" "row" $
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
        elClass "h1" "text-xs-center" $ text "Sign Up - TODO"
