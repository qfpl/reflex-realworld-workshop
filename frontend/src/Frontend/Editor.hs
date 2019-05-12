{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables                                                                     #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                             #-}
module Frontend.Editor where

import Control.Lens
import Reflex.Dom.Core

import qualified Data.Map               as Map
import qualified Data.Set               as Set
import           Obelisk.Route.Frontend (pattern (:/), R, SetRoute, setRoute)

import qualified Common.Conduit.Api.Articles.Article    as Article
import           Common.Conduit.Api.Articles.Attributes (ArticleAttributes (..), CreateArticle)
import           Common.Conduit.Api.Namespace           (Namespace (Namespace), unNamespace)
import qualified Common.Conduit.Api.User.Account        as Account
import           Common.Route                           (DocumentSlug (..), FrontendRoute (..))
import qualified Frontend.Conduit.Client                as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                         (buttonClass)

editor
  :: forall t m js s
  . ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
editor = elClass "div" "editor-page" $ do
  elClass "div" "container" $
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
        elClass "h1" "text-xs-center" $ text "TODO"
