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

module Main where

import Celebrity.Types
import Control.Lens ((^.), at, foldMapOf)
import Control.Monad
import Control.Monad.Fix
import Data.Function
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import JSDOM (currentWindowUnchecked)
import JSDOM.Custom.Window (getLocalStorage)
import JSDOM.Storage (getItem, setItem)
import Language.Javascript.JSaddle
import Reflex.Dom
import Reflex.Firebase
import Reflex.Router
import qualified URI.ByteString as U

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

widgetHead :: (DomBuilder t m) => m ()
widgetHead = do
  el "title" $ text "Celebrity"
  elAttr "meta" (mconcat ["name" =: "viewport", "content" =: "width=device-width, initial-scale=1"]) blank
  elAttr
    "link"
    ( mconcat
        [ "rel" =: "stylesheet",
          "type" =: "text/css",
          "href" =: "https://cdn.jsdelivr.net/npm/bulma@0.8.2/css/bulma.min.css"
        ]
    )
    blank

main :: IO ()
main =
  mainWidgetWithHead widgetHead $ do
    script1 <- fst <$> elAttr' "script" ("src" =: "https://www.gstatic.com/firebasejs/7.14.1/firebase.js") blank
    void $ widgetHold loading
      $ ffor (domEvent Load script1)
      $ \_ -> flip runFirebase firebaseConfig $ mdo
        storage <- currentWindowUnchecked >>= getLocalStorage
        player <- getItem @_ @Text @Player storage "player" >>= \case
          Nothing -> do
            p <- Player <$> liftJSM randText
            setItem @_ @Text storage "player" p
            pure p
          Just p -> pure p
        leaveGameE <- navbar inGameD
        inGameD <- elAttr "section" ("class" =: "hero is-link is-fullheight-with-navbar") $ do
          elAttr "div" ("class" =: "hero-body") $ do
            elAttr "div" ("class" =: "container has-text-centered") $ mdo
              urlD <- do
                flatNewGameE <- switchHold never newGameE
                let gameE = leftmost [Nothing <$ leaveGameE, Just <$> flatNewGameE]
                route $ fmap (maybe "?" (\(Id i) -> "?id=" <> i)) gameE
              let inGameD = ffor urlD $
                    \u -> case u ^. U.queryL . U.queryPairsL of
                      [("id", i)] -> Just $ Id $ E.decodeUtf8 i
                      _ -> Nothing
              newGameE <- dyn
                $ ffor inGameD
                $ \case
                  Just i -> do
                    inGameWidget i player
                    pure never
                  Nothing -> newGameWidget
              pure inGameD
        pure ()

navbar ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m
  ) =>
  Dynamic t (Maybe Id) ->
  m (Event t ())
navbar inGameD = elAttr "nav" (mconcat ["class" =: "navbar is-fixed-top is-light", "role" =: "navigation"]) $ do
  elAttr "div" ("class" =: "navbar-brand") $ do
    elAttr "div" ("class" =: "navbar-item") $ do
      el "h1" $ text "Celebrity"
    leaveGameE <- dyn $ ffor inGameD $ \case
      Nothing -> never <$ blank
      Just _ -> elAttr "div" (mconcat ["class" =: "navbar-item", "style" =: "margin-left:auto;"]) $ do
        domEvent Click . fst <$$> elAttr' "button" ("class" =: "button is-danger") $ text "Leave game"
    switchHold never leaveGameE

loading :: (DomBuilder t0 m) => m ()
loading = elAttr "div" ("class" =: "message is-info") $ do
  elAttr "div" ("class" =: "message-body") $ text "Loading"

newGameWidget ::
  forall t m.
  ( DomBuilder t m,
    PostBuild t m,
    MonadFirebase m,
    PerformEvent t m,
    MonadJSM (Performable m)
  ) =>
  m (Event t Id)
newGameWidget = do
  elAttr "h1" ("class" =: "title") $ text "Celebrity"
  newGameClickE <-
    domEvent Click . fst <$$> elAttr' "button" ("class" =: "button is-large is-fullwidth is-success") $
      text "Start a new game"
  db <- askDb
  performEvent
    $ ffor newGameClickE
    $ \(_ :: ()) -> liftJSM $ do
      (i, item) <- (,) <$> (Id <$> randText) <*> initialState
      set' db R1 (i, item)
      pure i

shouldWidgetKeepLocalState :: GameState -> GameState -> Bool
shouldWidgetKeepLocalState (WritingQuestions _) (WritingQuestions _) = True
shouldWidgetKeepLocalState _ _ = False

inGameWidget ::
  ( DomBuilder t m,
    MonadHold t m,
    PerformEvent t m,
    TriggerEvent t m,
    PostBuild t m,
    MonadFirebase m,
    MonadFirebase (Performable m),
    MonadFix m
  ) =>
  Id ->
  Player ->
  m ()
inGameWidget i p = do
  mfbD <- subscribeDoc @R @FireBaseState R1 i >>= maybeDyn
  void $ dyn $ ffor mfbD $ \case
    Nothing -> loading
    Just fbD -> do
      listenableChanges <- holdUniqDynBy (shouldWidgetKeepLocalState `on` _fireBaseStateState) fbD
      void $ dyn $ (widgetPicker fbD i p . _fireBaseStateState) <$> listenableChanges

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
  Id ->
  Player ->
  GameState ->
  m ()
widgetPicker fbD i p = \case
  WritingQuestions s -> do
    saveWordListE <- writingQuestionsWidget fbD p s
    _ <- transactionUpdate R1 $ ffor saveWordListE $ \wl ->
      ( i,
        HandleStale $ \(fbs@FireBaseState {_fireBaseStateState}) -> case _fireBaseStateState of
          WritingQuestions _ ->
            Just $
              fbs
                & fireBaseStateState
                . _WritingQuestions
                . writingQuestionStatePlayersWords
                . at p
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
writingQuestionsWidget fbD player (WritingQuestionState {_writingQuestionStatePlayersWords}) = mdo
  let totalWordsSubmittedD =
        ffor fbD $
          getSum
            . foldMapOf
              ( fireBaseStateState
                  . _WritingQuestions
                  . writingQuestionStatePlayersWords
              )
              (foldMap (Sum . NE.length))
      initWords = maybe [] NE.toList $ Map.lookup player _writingQuestionStatePlayersWords
  showWordListD <- toggle (null initWords) toggleShowWordListE
  toggleShowWordListEE <- dyn $ ffor showWordListD $ \case
    False -> do
      elAttr "div" ("class" =: "content") $ do
        elAttr "h2" ("class" =: "is-large") $ dynText $ ffor totalWordsSubmittedD $ \t -> (T.pack $ show t) <> " word(s) submitted so far."
      editWordListClickE <-
        domEvent Click . fst <$$> elAttr' "button" ("class" =: "button is-success") $
          text "Edit Words"
      pure $ Nothing <$ editWordListClickE
    True -> do
      words <- sample $ current wordD
      elAttr "p" ("class" =: "title") $ text "Add some words then click save"
      wordListD <- el "div" $ wordList words
      el "br" blank
      saveClickE <-
        tagPromptlyDyn wordListD . domEvent Click . fst
          <$$> elAttr' "button" ("class" =: "button is-success")
          $ text "Save"
      pure $ Just <$> saveClickE
  toggleShowWordListE <- switchHold never toggleShowWordListEE
  let wordListE = fmapMaybe id toggleShowWordListE
  wordD <- holdDyn initWords wordListE
  pure wordListE

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
  let initWordsMap =
        Map.fromList $ zip [0 ..] $
          if null initWords then [""] else initWords
      wordListDeletE = switchDyn $ leftmost . Map.elems . fmap snd <$> wordListInputs
      wordListAddE = domEvent Click addWordButton
      onWordListMod (Left _) xs =
        let x = maybe (-1 :: Int) fst (Map.lookupMax xs)
         in Map.insert (succ x) "" xs
      onWordListMod (Right i) xs = Map.delete i xs
  wordListD <-
    foldDyn onWordListMod initWordsMap $
      leftmost [Left <$> wordListAddE, Right <$> wordListDeletE]
  wordListInputs <- elAttr "div" ("class" =: "content is-large")
    $ el "ol"
    $ listWithKey wordListD
    $ \i v ->
      el "li" $ elAttr "div" ("class" =: "field has-addons") $ do
        initialVal <- sample $ current v
        inputE <-
          elAttr "div" ("class" =: "control is-expanded") $
            _inputElement_value
              <$> inputElement
                ( def
                    & inputElementConfig_initialValue .~ initialVal
                    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: "input")
                )
        removeClickE <-
          elAttr "div" ("class" =: "control") $
            (i <$$ (domEvent Click . fst <$$> elAttr' "button" ("class" =: "button is-danger") $ text "X"))
        pure (inputE, removeClickE)
  (addWordButton, _) <-
    elAttr' "button" ("class" =: "button") $
      text "Add another word"
  pure $ join $ mconcat . fmap (fmap pure) . Map.elems . fmap fst <$> wordListInputs
