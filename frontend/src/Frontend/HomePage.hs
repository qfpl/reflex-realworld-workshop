{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables                                                                     #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                             #-}
module Frontend.HomePage where

import Reflex.Dom.Core

import Control.Monad.Fix      (MonadFix)
import Data.Functor           (void)
import Obelisk.Route          (pattern (:/), R)
import Obelisk.Route.Frontend (RouteToUrl, SetRoute)
import Servant.Common.Req     (QParam (QNone), reqSuccess)

import           Common.Conduit.Api.Articles.Articles (Articles (..))
import           Common.Route                         (FrontendRoute (..))
import           Frontend.ArticlePreview              (articlesPreview)
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (routeLinkClass)

homePage
  :: forall t m s js
  . ( PostBuild t m
     , DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , Prerender js t m
     )
  => m ()
homePage = elClass "div" "home-page" $ do
  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"

  elClass "div" "container page" $ elClass "div" "row" $ do
    elClass "div" "col-md-9" $ do
      elClass "div" "feed-toggle" $
        elClass "ul" "nav nav-pills outline-active" $ do
          elClass "li" "nav-item" $ elClass "a" "nav-link disabled" $ text "Your Feed"
          elClass "li" "nav-item" $ elClass "a" "nav-link active" $ text "Global Feed"

    elClass "div" "col-md-3" $
      elClass "div" "sidebar" $ do
        el "p" $ text "TAGS TODO"
