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

articleContent
  :: forall t m js
  .  ( DomBuilder t m
     , Prerender js t m
     )
  => Dynamic t (Maybe Article.Article)
  -> m ()
articleContent articleDyn = prerender_ (text "Rendering Document...") $ do
  let htmlDyn = (fromMaybe "" . fmap (markDownToHtml5 . Article.body)) <$> articleDyn
  elClass "div" "row article-content" $ do
    d <- askDocument
    -- We have to sample the initial value to set it on creation
    htmlT <- sample . current $ htmlDyn
    e <- liftJSM $ do
      -- This wont execute scripts, but will allow users to XSS attack through
      -- event handling javascript attributes in any raw HTML that is let
      -- through the markdown renderer. But this is the simplest demo that
      -- mostly works. See https://github.com/qfpl/reflex-dom-template for a
      -- potentially more robust solution (we could filter out js handler attrs
      -- with something like that).
      -- It's worth noting that the react demo app does exactly what this does:
      -- https://github.com/gothinkster/react-redux-realworld-example-app/blob/master/src/components/Article/index.js#L60
      e <- createElement d ("div" :: String)
      setInnerHTML e htmlT
      pure e
    -- And make sure we update the html when the article changes
    performEvent_ $ (liftJSM . setInnerHTML e) <$> updated htmlDyn
    -- Put out raw element into our DomBuilder
    placeRawElement e

markDownToHtml5 :: Text -> Text
markDownToHtml5 t =
  case MMark.parse "" t of
    Left _  -> ""
    Right r -> TL.toStrict . L.renderText . MMark.render $ r
