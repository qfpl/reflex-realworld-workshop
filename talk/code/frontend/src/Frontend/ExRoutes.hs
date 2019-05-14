{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Frontend.ExRoutes where

import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Common.Route (ExampleSubRoute(..), FrontendRoute(..))
import Obelisk.Route.Frontend (R, RoutedT, SetRoute, subRoute_,askRoute, maybeRoute, routeLink, pattern (:/), RouteToUrl)

exRoutes 
  :: ( DomBuilder t m, PostBuild t m, MonadHold t m
     , MonadFix m, SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m)
  => RoutedT t (Maybe (R ExampleSubRoute)) m ()
exRoutes = do
  currRouteDyn <- askRoute
  el "p" $ do
    text "Current Route: "
    display currRouteDyn 

  _ <- maybeRoute homeWidget $ subRoute_ $ \case 
    ExampleSubRoute_A -> aWidget
    ExampleSubRoute_B -> bWidget
  pure ()

homeWidget 
  :: ( DomBuilder t m, SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m) 
  => m ()
homeWidget = do
  routeLink 
    (FrontendRoute_ExRoutes :/ (Just $ ExampleSubRoute_A :/ ())) 
    (text "Goto A")

aWidget 
  :: ( DomBuilder t m, SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m) 
  => m ()
aWidget = do
  routeLink (FrontendRoute_ExRoutes :/ Nothing) $ text "Go Home"
  text " "
  routeLink 
    (FrontendRoute_ExRoutes :/ (Just $ ExampleSubRoute_B :/ ()))
    (text "Goto B")

bWidget :: (DomBuilder t m) => m ()
bWidget = text "Game Over"
