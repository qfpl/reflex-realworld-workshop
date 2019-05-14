{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.ExReader where

import Reflex.Dom.Core
import Data.Monoid (Endo(..),appEndo)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Bool (bool)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Fix (MonadFix)

exReader :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
exReader = mdo
  (_,updatesE) <- flip runReaderT clickCountDyn 
    . runEventWriterT 
    $ do
      addButton
      resetButton

  clickCountDyn <- foldDyn appEndo 0 updatesE
  text " Clicked: "
  display clickCountDyn
  text " times!"

addButton :: (DomBuilder t m, EventWriter t (Endo Int) m) => m ()
addButton = do
  addClickE   <- button "Click me"
  tellEvent $ Endo (+1) <$ addClickE 

resetButton 
  :: ( DomBuilder t m, EventWriter t (Endo Int) m
     , MonadReader (Dynamic t Int) m , PostBuild t m) 
  => m ()
resetButton = do
  clickCountDyn <- ask
  let disabledDyn = (== 0) <$> clickCountDyn
  resetClickE <- buttonDynDisabled disabledDyn "Reset me"
  tellEvent $ Endo (const 0) <$ resetClickE 

buttonDynDisabled 
  :: (DomBuilder t m, PostBuild t m) 
  => Dynamic t Bool 
  -> Text 
  -> m (Event t ())
buttonDynDisabled disabledDyn dt = do
  (buttElt,_) <- elDynAttr' "button" 
    (bool Map.empty ("disabled" =: "") <$> disabledDyn)
    (text dt)
  pure $ domEvent Click buttElt

