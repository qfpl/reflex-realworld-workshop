{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TypeApplications                                      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints                             #-}

module Frontend.Warmup where

import Control.Lens
import Reflex.Dom.Core

import           Control.Monad.Fix    (MonadFix)
import           Control.Monad.Reader (MonadReader, ask, runReaderT)
import           Data.Bool            (bool)
import qualified Data.Map             as Map
import           Data.Monoid.Endo     (Endo (Endo), appEndo)
import           Data.Text            (Text)

warmup :: ( DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
warmup = elClass "div" "container page" $ do
  warmup1
  warmup2
  warmup3
  warmup4

warmup1 :: ( DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
warmup1 = el "div" $ do
  el "h2" $ text "Warmup1"
  bClickE  <- button "Click Me"
  countDyn <- foldDyn (\_ old -> old + 1) (0 :: Int) bClickE
  text " "
  display countDyn


warmup2 :: forall t m. ( DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
warmup2 = do
  el "h2" $ text "Warmup2"
  addClickE   <- button "Click me"
  resetClickE <- button "Reset me"

  let addE     = Endo (+1)      <$ addClickE
  let resetE   = Endo (const 0) <$ resetClickE
  let updatesE = addE <> resetE :: Event t (Endo Int)

  clickCountDyn <- foldDyn appEndo 0 updatesE
  text " "
  display clickCountDyn

addButton :: (DomBuilder t m, EventWriter t (Endo Int) m) => m ()
addButton = do
  addClickE   <- button "Click me"
  tellEvent $ Endo (+1) <$ addClickE

resetButton1 :: (DomBuilder t m, EventWriter t (Endo Int) m) => m ()
resetButton1 = do
  resetClickE <- button "Reset me"
  tellEvent $ Endo (const 0) <$ resetClickE

warmup3 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
warmup3 = do
  el "h2" $ text "Warmup3"
  (_,updatesE) <- runEventWriterT $ do -- Because we call runEventWriterT here, this do block has the
    addButton                          -- type EventWriterT t (Endo Int) m a
    resetButton1                       -- so the result after running is m ((),Event t (Endo t Int))
  clickCountDyn <- foldDyn appEndo 0 updatesE
  text " "
  display clickCountDyn

resetButton2
  :: ( DomBuilder t m, EventWriter t (Endo Int) m
     , MonadReader (Dynamic t Int) m , PostBuild t m)
  => m ()
resetButton2 = do
  clickCountDyn <- ask                                    -- The type of this is m (Dynamic t Int)
  let disabledDyn = (== 0) <$> clickCountDyn              -- We map over it to turn it to a boolean
  resetClickE <- buttonDynDisabled disabledDyn "Reset me" -- And we pass that disabled dynamic into a sub widget
  tellEvent $ Endo (const 0) <$ resetClickE               -- And do our usual tellEvent

buttonDynDisabled
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Bool
  -> Text
  -> m (Event t ())
buttonDynDisabled disabledDyn dt = do
  (buttElt,_) <- elDynAttr' "button"
    (bool classAttr (classAttr <> ("disabled" =: "")) <$> disabledDyn)
    (text dt)
  pure $ domEvent Click buttElt
  where
    classAttr = "class"=:"btn"

warmup4 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
warmup4 = do
  el "h2" $ text "Warmup4"
  rec (_,updatesE) <- -- Rec allows us to reference clickCountDyn when running the reader
        flip runReaderT clickCountDyn
        . runEventWriterT
        $ do             -- Because we call runEventWriterT and runReaderT here, this do block has the
          addButton      -- type ReaderT (Dynamic t Int) (EventWriterT t (Endo Int) m) a
          resetButton2   -- so the result after the runs is m ((),Event t (Endo t Int))
      clickCountDyn <- foldDyn appEndo 0 updatesE
  text " "
  display clickCountDyn
