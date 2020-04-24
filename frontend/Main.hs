{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- {"tag":"WritingQuestions","contents":{"_writingQuestionStatePlayersWords":[[{"unPlayer":"b"},["baby", "baby", "baby"]]]}}
-- {"tag":"WaitingForRound"}
module Main where

import Celebrity.Types
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Void
import GHC.Generics (Generic)
import Language.Javascript.JSaddle
import Reflex.Dom
import Reflex.Firebase

firebaseConfig :: Config
firebaseConfig =
  Config
    { apiKey = "AIzaSyAE0bLrbn3ZipK0b5Ns8ng2-wVNfPF82mk",
      authDomain = "celebrity-1126e.firebaseapp.com",
      databaseURL = "https://celebrity-1126e.firebaseio.com",
      projectId = "celebrity-1126e",
      storageBucket = "celebrity-1126e.appspot.com",
      messagingSenderId = "1985534572",
      appId = "1:1985534572:web:0125b3f478ee74b94ed649"
    }

(<$$) :: (Functor f, Functor g) => b -> f (g a) -> f (g b)
(<$$) = fmap . fmap . const

infixl 4 <$$

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 4 <$$>

main :: IO ()
main =
  mainWidget $ do
    script1 <- fst <$> elAttr' "script" ("src" =: "https://www.gstatic.com/firebasejs/7.14.1/firebase.js") blank
    void $ widgetHold (el "pre" $ text "Loading")
      $ ffor (domEvent Load script1)
      $ \_ -> flip runFirebase firebaseConfig $ do
        fbD <- subscribeDoc @R @FireBaseState R1 (Id "jcmKQtFxX63H8QoPvII8")
        let widgetPicker = \case
              WritingQuestions s -> writingQuestionsWidget fbD (Player "b") s
              WaitingForRound -> waitingForRorRoundWidget
            onJustChange _ Nothing Nothing = True
            onJustChange f (Just a) (Just b) = f (_fireBaseStateState a) (_fireBaseStateState b)
            onJustChange _ _ _ = False
            keepLocalState (WritingQuestions _) (WritingQuestions _) = True
            keepLocalState _ _ = False
        listenableChanges <- holdUniqDynBy (onJustChange keepLocalState) fbD
        void $ dyn $
          maybe (el "pre" $ text "Loading") (widgetPicker . _fireBaseStateState) <$> listenableChanges

waitingForRorRoundWidget ::
  ( DomBuilder t m
  ) =>
  m ()
waitingForRorRoundWidget = do
  el "h1" $ text "Waiting for Round"

writingQuestionsWidget ::
  forall t m.
  ( MonadFix m,
    DomBuilder t m,
    MonadHold t m,
    PostBuild t m,
    Reflex t,
    MonadFirebase m,
    MonadFirebase (Performable m),
    TriggerEvent t m,
    PerformEvent t m
  ) =>
  Dynamic t (Maybe FireBaseState) ->
  Player ->
  WritingQuestionState ->
  m ()
writingQuestionsWidget fbD player (WritingQuestionState {_writingQuestionStatePlayersWords}) = mdo
  let totalWordsSubmittedD =
        ffor fbD $
          foldMapOf
            ( _Just
                . fireBaseStateState
                . _WritingQuestions
                . writingQuestionStatePlayersWords
            )
            (foldMap (Sum . NE.length))
      initWords = maybe [] NE.toList $ Map.lookup player _writingQuestionStatePlayersWords
  el "div" $ display totalWordsSubmittedD
  wordListD <- el "div" $ wordList initWords
  saveClickE :: Event t () <- domEvent Click . fst <$$> el' "button" $ text "Save Word List"
  updatingWordListD <- transactionUpdate R1 $ ffor (tagPromptlyDyn wordListD saveClickE) $ \wl ->
    ( Id "jcmKQtFxX63H8QoPvII8",
      HandleStale $ \(fbs@FireBaseState {_fireBaseStateState}) -> case _fireBaseStateState of
        WritingQuestions _ ->
          Just $
            fbs
              & fireBaseStateState
              . _WritingQuestions
              . writingQuestionStatePlayersWords
              . at player
              .~ NE.nonEmpty (filter (not . T.null) wl)
        _ -> Nothing
    )
  pure ()

wordList ::
  forall m t.
  ( MonadFix m,
    DomBuilder t m,
    MonadHold t m,
    PostBuild t m,
    Reflex t
  ) =>
  [Text] ->
  m (Dynamic t [Text])
wordList initWords = mdo
  let initWordsMap = Map.fromList $ zip [0 ..] initWords
      wordListDeletE = switchDyn $ leftmost . Map.elems . fmap snd <$> wordListInputs
      wordListAddE = domEvent Click addWordButton
      onWordListMod (Left _) xs =
        let x = maybe (-1) fst (Map.lookupMax xs)
         in Map.insert (succ x) "" xs
      onWordListMod (Right i) xs = Map.delete i xs
  wordListD <-
    foldDyn onWordListMod initWordsMap $
      leftmost [Left <$> wordListAddE, Right <$> wordListDeletE]
  wordListInputs <- el "ol"
    $ listWithKey wordListD
    $ \i v ->
      el "li" $ do
        initialVal <- sample $ current v
        (,)
          <$> (_inputElement_value <$> inputElement (def & inputElementConfig_initialValue .~ initialVal))
          <*> (i <$$ (domEvent Click . fst <$$> el' "button" $ text "X"))
  (addWordButton, _) <- el' "button" $ text "Add"
  pure $ join $ mconcat . fmap (fmap pure) . Map.elems . fmap fst <$> wordListInputs

firebaseTest ::
  ( MonadJSM m,
    PerformEvent t m,
    PostBuild t m,
    TriggerEvent t m,
    MonadHold t m,
    DomBuilder t m,
    MonadFix m,
    MonadJSM (Performable m)
  ) =>
  m ()
firebaseTest = do
  script1 <- fst <$> elAttr' "script" ("src" =: "https://www.gstatic.com/firebasejs/7.14.1/firebase.js") blank
  void $ widgetHold (el "pre" $ text "Loading")
    $ ffor (domEvent Load script1)
    $ \_ -> flip runFirebase firebaseConfig $ do
      pbE <- getPostBuild
      randE <- performEvent $ liftJSM randRevision <$ pbE
      fbD <- subscribe (Query @R @FireBaseState R1 [])
      --add @R @FireBaseState R1 (FireBaseState (WritingQuestions (WritingQuestionState mempty)) <$> randE)
      (transD1, transD2) <-
        (,)
          <$> ( transactionUpdate R1
                  $ ffor randE
                  $ \r ->
                    ( Id "jcmKQtFxX63H8QoPvII8",
                      OnlyCurrent $
                        FireBaseState
                          (WritingQuestions (WritingQuestionState (Player "a" =: (NE.fromList ["oh yeah"]))))
                          (Revision "3775987453")
                    )
              )
          <*> ( transactionUpdate R1
                  $ ffor randE
                  $ \r ->
                    ( Id "jcmKQtFxX63H8QoPvII8",
                      OnlyCurrent $
                        FireBaseState
                          (WritingQuestions (WritingQuestionState (Player "b" =: (NE.fromList ["baby"]))))
                          (Revision "3775987453")
                    )
              )
      el "pre" $ display transD1
      el "pre" $ display transD2
      el "ol" $ simpleList fbD $ \d -> el "li" $ display d
      pure ()
