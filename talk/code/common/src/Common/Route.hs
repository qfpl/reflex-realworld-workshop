{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity
import Data.Functor.Sum

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_ExDom :: FrontendRoute ()
  FrontendRoute_ExFoldDyn :: FrontendRoute ()
  FrontendRoute_ExMergeEvents :: FrontendRoute ()
  FrontendRoute_ExEventWriter :: FrontendRoute ()
  FrontendRoute_ExReader :: FrontendRoute ()
  FrontendRoute_ExRoutes :: FrontendRoute (Maybe (R ExampleSubRoute))
  FrontendRoute_ExWorkflow :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

data ExampleSubRoute :: * -> * where
  ExampleSubRoute_A :: ExampleSubRoute ()
  ExampleSubRoute_B :: ExampleSubRoute ()

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_ExDom -> PathSegment "dom" $ unitEncoder mempty
      FrontendRoute_ExFoldDyn -> PathSegment "fold_dyn" $ unitEncoder mempty
      FrontendRoute_ExMergeEvents -> PathSegment "merge_events" $ unitEncoder mempty
      FrontendRoute_ExEventWriter -> PathSegment "event_writer" $ unitEncoder mempty
      FrontendRoute_ExReader -> PathSegment "reader" $ unitEncoder mempty
      FrontendRoute_ExWorkflow -> PathSegment "workflow" $ unitEncoder mempty
      FrontendRoute_ExRoutes -> PathSegment "routes" $ 
        maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
          ExampleSubRoute_A -> PathSegment "a" $ unitEncoder mempty
          ExampleSubRoute_B -> PathSegment "b" $ unitEncoder mempty
      

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''ExampleSubRoute
  ]
