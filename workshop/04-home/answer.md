# Answer for the home page.

Because the actual example has the feed built in it is a bit more complicated. So here is the answer as far as the exercise goes.

```haskell
{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TemplateHaskell                                       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                             #-}
module Frontend.HomePage where

import Control.Lens    hiding (element)
import Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Data.Foldable          (fold)
import           Data.Functor           (void)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as NEL
import qualified Data.Map               as Map
import           Data.Proxy             (Proxy (Proxy))
import           Data.Text              (Text)
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute)
import           Servant.Common.Req     (QParam (QNone))

import           Common.Conduit.Api.Articles.Articles (Articles (..))
import           Common.Conduit.Api.Namespace         (Namespace (..), unNamespace)
import           Common.Conduit.Api.User.Account      (Token (..))
import           Common.Route                         (FrontendRoute (..))
import           Frontend.ArticlePreview              (articlesPreview)
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (routeLinkClass, buttonDynClass, buttonClass)

data HomePageSelected = GlobalSelected | TagSelected Text deriving Show
makePrisms ''HomePageSelected

homePageSelected :: a -> (Text -> a) -> HomePageSelected -> a
homePageSelected globalSel tagSel = \case
  GlobalSelected -> globalSel
  TagSelected a  -> tagSel a

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
homePage = elClass "div" "home-page" $ mdo
  _tokDyn :: Dynamic t (Maybe Token) <- reviewFrontendState loggedInToken
  _pbE <- getPostBuild

  -- allTags
  -- :: (Reflex t, Applicative m, Prerender js t m)
  -- => Event t ()
  -- -> m (ClientRes t (Namespace "tags" [Text]))
  (allTagsSuccessE,_,_) <- Client.allTags (_pbE <> void (updated _tokDyn))
  allTagsDyn <- holdDyn (Namespace []) allTagsSuccessE
  selectionDyn <- holdDyn GlobalSelected (NEL.head <$> selectionChangeE)

  -- listArticles
  -- :: (Reflex t, Applicative m, Prerender js t m)
  -- => Dynamic t (Maybe Token)
  -- -> Dynamic t (QParam Integer) -- limit
  -- -> Dynamic t (QParam Integer) -- offset
  -- -> Dynamic t [Text]           -- tags
  -- -> Dynamic t [Text]           -- favourited
  -- -> Dynamic t [Text]           -- authors
  -- -> Event t ()                 -- submit
  -- -> m (ClientRes t Articles)
  (articlesSuccessE,_,artsLoadingDyn) <- Client.listArticles
    _tokDyn
    (constDyn QNone)
    (constDyn QNone)
    (homePageSelected [] (:[]) <$> selectionDyn)
    (constDyn [])
    (constDyn [])
    (fold [_pbE, void (updated _tokDyn), void (updated selectionDyn)])
  artsDyn <- holdDyn (Articles [] 0) articlesSuccessE

  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"

  (_, selectionChangeE) <- runEventWriterT $ elClass "div" "container page" $ elClass "div" "row" $ do
    elClass "div" "col-md-9" $ do
      elClass "div" "feed-toggle" $
        elClass "ul" "nav nav-pills outline-active" $ do
          let globalClassDyn = ("nav-link" <>) . (^._GlobalSelected.to (const " active")) <$> selectionDyn
          globalSelected <- elClass "li" "nav-item" $ buttonDynClass globalClassDyn (constDyn False) $ text "Global Feed"
          tellEvent $ pure GlobalSelected <$ globalSelected
          void . dyn . ffor selectionDyn $ homePageSelected blank $ \t ->
            void . elClass "li" "nav-item" $ buttonClass "nav-link active" (constDyn False) $ text t

      articlesPreview artsLoadingDyn artsDyn

    elClass "div" "col-md-3" $
      elClass "div" "sidebar" $ do
        el "p" $ text "Popular Tags"
        elClass "div" "tag-list" $ do
          -- TODO: Combine simpleList and tagPill to print out all tags
          --el "p" $ text "TODO - Load tags"
          void $ list (Map.fromList . fmap (\a -> (a,a)) . unNamespace <$> allTagsDyn) tagPill

  pure ()

tagPill
  :: forall t m
  . (DomBuilder t m, PostBuild t m, EventWriter t (NonEmpty HomePageSelected) m)
  => Dynamic t Text
  -> m ()
tagPill tDyn = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ ("class" =: "tag-pill tag-default" <> "href" =: "")
  (e, _) <- element "a" cfg $ dynText tDyn

  -- We'll gloss over this for now. But you can read this as:
  -- When the button is clicked, tag the event with the current value of the tDyn text.
  -- And then wrap it up in a Non empty list of HomeSelectedEvents (a list because EventWriter needs a semigroup)
  tellEvent $ pure . TagSelected <$> current tDyn <@ domEvent Click e
```
