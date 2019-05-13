{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications                                                   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                             #-}

module Frontend.Settings where

import Control.Lens
import Reflex.Dom.Core

import           Control.Monad          (mfilter)
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as Text
import           Obelisk.Route.Frontend (pattern (:/), R, SetRoute, setRoute)

import           Common.Conduit.Api.Namespace    (Namespace (Namespace), unNamespace)
import qualified Common.Conduit.Api.User.Account as Account
import           Common.Conduit.Api.User.Update  (UpdateUser (UpdateUser))
import           Common.Route                    (FrontendRoute (..), Username (..))
import qualified Frontend.Conduit.Client         as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                  (buttonClass)

settings
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , EventWriter t (NonEmpty e) m
     , AsFrontendEvent e
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
settings = elClass "div" "settings-page" $ do
  elClass "div" "container page" $
    elClass "div" "row" $
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
        elClass "h1" "text-xs-center" $ text "Your Settings"
        el "form" $ do
          -- TODO : Load the existing data
          -- getCurrentUser
          -- :: (Reflex t, Applicative m, Prerender js t m)
          -- => Dynamic t (Maybe Token)
          -- -> Event t ()
          -- -> m (ClientRes t (Namespace "user" Account))

          el "fieldset" $ do
            _urlI <- elClass "fieldset" "form-group" $
              inputElement $ def
                & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                  [("class","form-control")
                  ,("placeholder","URL of profile picture")
                  ]
                -- Note that we set the form val from AJAX returned data
                & inputElementConfig_setValue .~ never
            _usernameI <- elClass "fieldset" "form-group" $
              inputElement $ def
                & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                  [("class","form-control")
                  ,("placeholder","Your name")
                  ]
                & inputElementConfig_setValue .~ never
            _bioI <- elClass "fieldset" "form-group" $
              textAreaElement $ def
                & textAreaElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                  [("class","form-control")
                  ,("placeholder","Short bio about you")
                  ,("rows","8")
                  ]
                & textAreaElementConfig_setValue .~ never

            _emailI <- elClass "fieldset" "form-group" $
              inputElement $ def
                & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                  [("class","form-control")
                  ,("placeholder","Email")
                  ,("type","input")
                  ]
                & inputElementConfig_setValue .~ never
            _passwordI <- elClass "fieldset" "form-group" $
              inputElement $ def
                & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                  [("class","form-control")
                  ,("placeholder","Password")
                  ,("type","password")
                  ]
            _updateE <- buttonClass "btn btn-lg btn-primary pull-xs-right" (constDyn False) $ text "Update Settings"

            -- TODO: submit form, redirect to profile when successful
            pure ()

          el "hr" blank

          -- TODO Logout button should do something
          _logoutClick <- buttonClass "btn btn-outline-danger" (constDyn False) $ text "Logout"
          pure ()
