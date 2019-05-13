{-# LANGUAGE DataKinds, FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RecursiveDo, ScopedTypeVariables                                 #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                             #-}
module Frontend.Login where

import Control.Lens
import Reflex.Dom.Core hiding (Namespace)

import           Control.Monad.Fix      (MonadFix)
import           Data.Foldable          (traverse_)
import           Data.Functor           (void)
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute, routeLink, setRoute)

import           Common.Conduit.Api.User.Account      (Account)
import           Common.Conduit.Api.Namespace         (Namespace (Namespace), unNamespace)
import           Common.Conduit.Api.Users.Credentials (Credentials (Credentials))
import           Common.Route                         (FrontendRoute (..))
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass)

login
  :: forall js t m s e
  . ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     , EventWriter t (NonEmpty e) m
     , AsFrontendEvent e
     , HasLoggedInAccount s
     , HasFrontendState t s m
     )
  => m ()
login = elClass "div" "auth-page" $ do
  elClass "div" "container-page" $ do
    elClass "div" "row" $ do
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
        elClass "h1" "text-xs-center" $ text "Sign in"

        -- Put a link here that goes to signup
        elClass "p" "text-xs-center" $
          routeLink (FrontendRoute_Register :/ ()) $ text "Need an account?"

        elClass "ul" "error-messages" $
          blank

        -- A form for two inputs
        el "form" $ mdo
          _emailI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Email")
                ])
          _passI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Password")
                , ("type","password")
                ])

          _submitE <- buttonClass "btn btn-lg btn-primary pull-xs-right" (constDyn False) $ text "Sign in"

          -- We need to call this functior
          -- Client.login
          --   :: Dynamic t (Namespace "user" Credentials)
          --   -> Event t ()
          --   -> m (Event t (Namespace "user" Account), Event t ClientError, Dynamic t Bool)

          -- Creating a pure version of the Credentials:
          let _creds :: Namespace "user" Credentials = Namespace $ Credentials "username" "password"

          pure ()

        -- Our end goal is to login by replace never with an appropriate action
        tellEvent $ pure . (_LogIn #) <$> (never :: Event t Account)

        pure ()
