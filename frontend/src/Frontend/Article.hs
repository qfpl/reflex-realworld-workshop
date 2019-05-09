{-# LANGUAGE FlexibleContexts, LambdaCase, MonoLocalBinds, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RecursiveDo, ScopedTypeVariables                                      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                            #-}

module Frontend.Article where

import Control.Lens
import Reflex.Dom.Core hiding (Element)

import           Control.Monad.Fix      (MonadFix)
import           Data.Default           (def)
import           Data.Foldable          (fold)
import           Data.Functor           (void)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            (Endo (Endo), appEndo)
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as TL
import           GHCJS.DOM.Document     (createElement)
import           GHCJS.DOM.Element      (setInnerHTML)
import           GHCJS.DOM.Types        (liftJSM)
import qualified Lucid                  as L
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, Routed, SetRoute, askRoute, routeLink)
import           Servant.Common.Req     (reqSuccess)
import qualified Text.MMark             as MMark


import qualified Common.Conduit.Api.Articles.Article       as Article
import qualified Common.Conduit.Api.Articles.Comment       as Comment
import qualified Common.Conduit.Api.Articles.CreateComment as CreateComment
import           Common.Conduit.Api.Namespace              (Namespace (..), unNamespace)
import qualified Common.Conduit.Api.Profiles.Profile       as Profile
import qualified Common.Conduit.Api.User.Account           as Account
import           Common.Route                              (DocumentSlug (..), FrontendRoute (..),
                                                            Username (..))
import           Frontend.ArticlePreview                   (profileImage, profileRoute)
import qualified Frontend.Conduit.Client                   as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                            (buttonClass, routeLinkClass, routeLinkDynClass,
                                                            showText)

article
  :: forall t m js s
  .  ( DomBuilder t m
     , Prerender js t m
     , Routed t DocumentSlug m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
article = elClass "div" "article-page" $ do
  elClass "div" "banner" $
    elClass "div" "container" $ do
      el "h1" $ text "TODO"
  elClass "div" "container page" $ do
    elClass "h1" "text-xs-center" $ text "TODO"
