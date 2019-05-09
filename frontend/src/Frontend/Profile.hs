{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms                                                               #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                   #-}
module Frontend.Profile where

import Reflex.Dom.Core

import Control.Monad.Fix      (MonadFix)
import Data.Bool              (bool)
import Data.Functor           (void)
import Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, RoutedT, SetRoute, askRoute)
import Servant.Common.Req     (QParam (QNone), reqSuccess)

import           Common.Conduit.Api.Articles.Articles (Articles (..))
import           Common.Conduit.Api.Namespace         (unNamespace)
import qualified Common.Conduit.Api.Profiles.Profile  as Profile
import           Common.Route                         (FrontendRoute (..), ProfileRoute (..), Username (..))
import           Frontend.ArticlePreview              (articlesPreview, profileImage)
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass, routeLinkDynClass)

profile
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , Prerender js t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t Username
  -> RoutedT t (Maybe (R ProfileRoute)) m ()
profile _usernameDyn = do
  elClass "div" "profile-page" $ do
  elClass "div" "user-info" $
    elClass "div" "container" $
      el "h1" $ text "TODO"
