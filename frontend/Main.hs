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

-- {"tag":"WritingQuestions","contents":{"_writingQuestionStatePlayersWords":[[{"unPlayer":"a"},["yeah","yeah","yeah"]],[{"unPlayer":"b"},["Devin!","do-do connector "]]]}}
-- {"tag":"WaitingForRound"}
module Main where

import Celebrity.Types
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Data.Function
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
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
    void $ widgetHold loading
      $ ffor (domEvent Load script1)
      $ \_ -> flip runFirebase firebaseConfig $ do
        mfbD <- subscribeDoc @R @FireBaseState R1 (Id "jcmKQtFxX63H8QoPvII8") >>= maybeDyn
        void $ dyn $ ffor mfbD $ \case
          Nothing -> loading
          Just fbD -> do
            listenableChanges <- holdUniqDynBy (shouldWidgetKeepLocalState `on` _fireBaseStateState) fbD
            void $ dyn $ (widgetPicker fbD . _fireBaseStateState) <$> listenableChanges

loading :: (DomBuilder t0 m) => m ()
loading = el "pre" $ text "Loading"

shouldWidgetKeepLocalState :: GameState -> GameState -> Bool
shouldWidgetKeepLocalState (WritingQuestions _) (WritingQuestions _) = True
shouldWidgetKeepLocalState _ _ = False

widgetPicker ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadFirebase (Performable m),
    MonadFirebase m
  ) =>
  Dynamic t FireBaseState ->
  GameState ->
  m ()
widgetPicker fbD = \case
  WritingQuestions s -> do
    saveWordListE <- writingQuestionsWidget fbD (Player "b") s
    _ <- transactionUpdate R1 $ ffor saveWordListE $ \wl ->
      ( Id "jcmKQtFxX63H8QoPvII8",
        HandleStale $ \(fbs@FireBaseState {_fireBaseStateState}) -> case _fireBaseStateState of
          WritingQuestions _ ->
            Just $
              fbs
                & fireBaseStateState
                . _WritingQuestions
                . writingQuestionStatePlayersWords
                . at (Player "b")
                .~ NE.nonEmpty (filter (not . T.null) wl)
          _ -> Nothing
      )
    pure ()
  WaitingForRound -> waitingForRorRoundWidget

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
    PostBuild t m
  ) =>
  Dynamic t FireBaseState ->
  Player ->
  WritingQuestionState ->
  m (Event t [Text])
writingQuestionsWidget fbD player (WritingQuestionState {_writingQuestionStatePlayersWords}) = do
  let totalWordsSubmittedD =
        ffor fbD $
          foldMapOf
            ( fireBaseStateState
                . _WritingQuestions
                . writingQuestionStatePlayersWords
            )
            (foldMap (Sum . NE.length))
      initWords = maybe [] NE.toList $ Map.lookup player _writingQuestionStatePlayersWords
  el "div" $ display totalWordsSubmittedD
  wordListD <- el "div" $ wordList initWords
  tagPromptlyDyn wordListD . domEvent Click . fst <$$> el' "button" $ text "Save Word List"

wordList ::
  forall m t.
  ( MonadFix m,
    DomBuilder t m,
    MonadHold t m,
    PostBuild t m
  ) =>
  [Text] ->
  m (Dynamic t [Text])
wordList initWords = mdo
  let initWordsMap = Map.fromList $ zip [0 ..] initWords
      wordListDeletE = switchDyn $ leftmost . Map.elems . fmap snd <$> wordListInputs
      wordListAddE = domEvent Click addWordButton
      onWordListMod (Left _) xs =
        let x = maybe (-1 :: Int) fst (Map.lookupMax xs)
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
                  $ \_ ->
                    ( Id "jcmKQtFxX63H8QoPvII8",
                      OnlyCurrent $
                        FireBaseState
                          (WritingQuestions (WritingQuestionState (Player "a" =: (NE.fromList ["oh yeah"]))))
                          (Revision "3775987453")
                    )
              )
          <*> ( transactionUpdate R1
                  $ ffor randE
                  $ \_ ->
                    ( Id "jcmKQtFxX63H8QoPvII8",
                      OnlyCurrent $
                        FireBaseState
                          (WritingQuestions (WritingQuestionState (Player "b" =: (NE.fromList ["baby"]))))
                          (Revision "3775987453")
                    )
              )
      el "pre" $ display transD1
      el "pre" $ display transD2
      _ <- el "ol" $ simpleList fbD $ \d -> el "li" $ display d
      pure ()
