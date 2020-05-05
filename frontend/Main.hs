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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Celebrity.Types
import Control.Lens ((%~), (^.), (^?), _1, _2, at, foldMapOf, view)
import Control.Monad (join, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class
import Data.Align (align)
import Data.Function (on)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.These (These (..), these)
import GHCJS.DOM.HTMLElement (focus)
import JSDOM (currentWindowUnchecked)
import JSDOM.Custom.Window (getLocalStorage)
--import JSDOM.HTMLElement (focus)
import JSDOM.Storage (getItem, setItem)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
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
              let mgameIdD = ffor urlD $
                    \u -> case u ^. U.queryL . U.queryPairsL of
                      [("id", i)] -> Just $ Id $ E.decodeUtf8 i
                      _ -> Nothing
              newGameE <- dyn
                $ ffor mgameIdD
                $ \case
                  Just i -> do
                    inGameWidget i player
                    pure never
                  Nothing -> newGameWidget
              pure mgameIdD
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
    MonadFix m,
    GhcjsDomSpace ~ DomBuilderSpace m
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
    MonadFirebase m,
    GhcjsDomSpace ~ DomBuilderSpace m
  ) =>
  Dynamic t FireBaseState ->
  Id ->
  Player ->
  GameState ->
  m ()
widgetPicker fbD i p gs = case gs of
  WritingQuestions s -> do
    writingQuestionE <- writingQuestionsWidget fbD p s
    void $ transactionUpdate R1 $ ffor (attachPromptlyDyn fbD writingQuestionE) $ \(fbs, wqe) ->
      case wqe of
        StartGame ->
          ( i,
            OnlyCurrent $
              fbs
                & fireBaseStateState
                %~ handleWritingQuestionsEvent p wqe
          )
        SavePhrases _ ->
          ( i,
            HandleStale $ \(fbs'@FireBaseState {_fireBaseStateState}) -> case _fireBaseStateState of
              WritingQuestions _ ->
                Just $
                  fbs'
                    & fireBaseStateState
                    %~ handleWritingQuestionsEvent p wqe
              _ -> Nothing
          )
  BetweenRound s -> do
    betweenRoundE <- betweenRoundWidget s
    void $ transactionUpdate R1 $ ffor (attachPromptlyDyn fbD betweenRoundE) $ \(fbs, bre) ->
      ( i,
        OnlyCurrent $
          fbs
            & fireBaseStateState
            %~ handleBetweenRoundEvent p bre
      )
  InRound s -> do
    inRoundE <- inRoundWidget p s
    void $ transactionUpdate R1 $ ffor (attachPromptlyDyn fbD inRoundE) $ \(fbs, ire) ->
      ( i,
        OnlyCurrent $
          fbs
            & fireBaseStateState
            %~ handleInRoundEvent ire
      )
  GameOver s -> do
    elAttr "div" ("class" =: "tile is-ancestor") $ do
      elAttr "div" ("class" =: "tile notification is-danger") $ do
        elAttr "p" ("class" =: "title is-5") $ text "Team 1"
        elAttr "p" ("class" =: "subtitle is-5") $ text $ T.pack $ show $ s ^. gameOverStateCelebrityState . celebrityStateTeam1Score
      elAttr "div" ("class" =: "tile notification is-primary") $ do
        elAttr "p" ("class" =: "title is-4") $ do
          let winngingTeam =
                if s ^. gameOverStateCelebrityState . celebrityStateTeam1Score > s ^. gameOverStateCelebrityState . celebrityStateTeam2Score
                  then "1"
                  else "2"
          text $ "Team " <> winngingTeam <> " Wins!"
      elAttr "div" ("class" =: "tile notification is-danger") $ do
        elAttr "p" ("class" =: "title is-5") $ text "Team 2"
        elAttr "p" ("class" =: "subtitle is-5") $ text $ T.pack $ show $ s ^. gameOverStateCelebrityState . celebrityStateTeam2Score

betweenRoundWidget ::
  ( DomBuilder t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m
  ) =>
  BetweenRoundState ->
  m (Event t BetweenRoundEvent)
betweenRoundWidget s = mdo
  showLoadingD <- toggle False myTurnE
  myTurnEE <- dyn $ ffor showLoadingD $ \case
    True -> never <$ loading
    False -> elAttr "div" ("class" =: "tile is-ancestor") $ do
      elAttr "div" ("class" =: "tile notification is-danger") $ do
        elAttr "p" ("class" =: "title is-5") $ text "Team 1"
        elAttr "p" ("class" =: "subtitle is-5") $ text $ T.pack $ show $ s ^. betweenRoundStateCelebrityState . celebrityStateTeam1Score
      myTurnClickE <- elAttr "div" ("class" =: "tile notification is-primary") $ do
        elAttr "p" ("class" =: "title is-4") $ text $ "Round " <> T.pack (show (s ^. betweenRoundStateCelebrityState . celebrityStateRoundNum))
        elAttr "p" ("class" =: "subtitle is-5") $ text $ (T.pack $ show $ length $ s ^. betweenRoundStateCelebrityState . celebrityStateFreePhrases) <> " phrase(s) left"
        elAttr "p" ("class" =: "title is-4") $ text $ "Team " <> T.pack (show $ s ^. betweenRoundStateCelebrityState . celebrityStateCurrentTeam) <> " is up!"
        domEvent Click . fst <$$> elAttr' "button" ("class" =: "button is-dark") $ text "My turn!"
      elAttr "div" ("class" =: "tile notification is-danger") $ do
        elAttr "p" ("class" =: "title is-5") $ text "Team 2"
        elAttr "p" ("class" =: "subtitle is-5") $ text $ T.pack $ show $ s ^. betweenRoundStateCelebrityState . celebrityStateTeam2Score
      pure $ MyTurn <$ myTurnClickE
  myTurnE <- switchHold never myTurnEE
  pure myTurnE

countdownTimer ::
  ( PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    MonadFix m,
    MonadHold t m,
    PostBuild t m
  ) =>
  Int ->
  m (Dynamic t Int, Event t ())
countdownTimer t = do
  tE <- tickLossyFromPostBuildTime 1
  tD <- foldDyn (const pred) t tE
  pure (tD, attachPromptlyDynWithMaybe (\left _ -> if left < 2 then Just () else Nothing) tD tE)

data PhraseListZipper a = PhraseListZipper [a] a [a]

inRoundWidget ::
  ( DomBuilder t m,
    PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    MonadFirebase (Performable m),
    MonadFirebase m,
    MonadFix m,
    MonadHold t m
  ) =>
  Player ->
  InRoundState ->
  m (Event t InRoundEvent)
inRoundWidget me s =
  if me == (s ^. inRoundStateCurrentPlayer)
    then do
      pbE <- getPostBuild
      randE <- performEvent $ liftIO (shuffle (Set.toList $ s ^. inRoundStateCelebrityState . celebrityStateFreePhrases)) <$ pbE
      roundOverD <- widgetHold (never <$ loading) $ ffor randE $ \shuffled -> mdo
        let mkZipper _ (guessedPhrases, Nothing) = (guessedPhrases, Nothing)
            mkZipper _ (guessedPhrases, Just (w NE.:| ws)) = (w : guessedPhrases, NE.nonEmpty ws)
        phrasesD <- foldDyn mkZipper ([], NE.nonEmpty shuffled) nextClickE
        let phrasesLeftD = maybe 0 NE.length . snd <$> phrasesD
        (timeleftD, hitZeroE) <- countdownTimer 60
        nextClickEE <- dyn $ ffor phrasesLeftD $ \phrasesLeft ->
          if phrasesLeft == 0
            then never <$ loading
            else do
              elAttr "div" ("class" =: "title is-3") $ display timeleftD
              elAttr "div" ("class" =: "subtitle is-6") $ text $ T.pack (show phrasesLeft) <> " phrase(s) left"
              elAttr "div" ("class" =: "title is-3") $ dynText $ maybe "" NE.head . snd <$> phrasesD
              domEvent Click . fst <$$> elAttr' "button" ("class" =: "button is-dark") $ text "Got it!"
        nextClickE <- switchHold never nextClickEE
        let outOfPhrasesE = fforMaybe (updated phrasesD) $
              \(guessedPhrases, mwds) -> maybe (Just guessedPhrases) (const Nothing) mwds
            hitZeroPhrasesE =
              attachPromptlyDynWith
                (\(guessedPhrases, _) _ -> guessedPhrases)
                phrasesD
                hitZeroE
        pure $ leftmost $ map (fmap CorrectGuesses) [outOfPhrasesE, hitZeroPhrasesE]
      pure $ switchDyn roundOverD
    else do
      elAttr "div" ("class" =: "title is-2") $ text "Pay attention!"
      pure never

writingQuestionsWidget ::
  forall t m.
  ( MonadFix m,
    DomBuilder t m,
    MonadHold t m,
    PostBuild t m,
    PerformEvent t m,
    MonadJSM (Performable m),
    GhcjsDomSpace ~ DomBuilderSpace m,
    TriggerEvent t m
  ) =>
  Dynamic t FireBaseState ->
  Player ->
  WritingQuestionState ->
  m (Event t WritingQuestionsEvent)
writingQuestionsWidget fbD player (WritingQuestionState {_writingQuestionStatePlayersPhrases}) = mdo
  let totalPhrasesSubmittedD =
        ffor2 fbD phraseD $ \fb wds ->
          let phrasesInLocalState = length wds
              phrasesNotInStateYet =
                maybe phrasesInLocalState (\myPhrases -> phrasesInLocalState - NE.length myPhrases) $ join $
                  fb
                    ^? fireBaseStateState
                      . _WritingQuestions
                      . writingQuestionStatePlayersPhrases
                      . at player
              totalPhrases =
                getSum $
                  foldMapOf
                    ( fireBaseStateState
                        . _WritingQuestions
                        . writingQuestionStatePlayersPhrases
                    )
                    (foldMap (Sum . NE.length))
                    fb
           in totalPhrases + phrasesNotInStateYet
      initPhrases = maybe [] NE.toList $ Map.lookup player _writingQuestionStatePlayersPhrases
  showPhraseListD <-
    toggle (null initPhrases) $
      fmapMaybe
        ( these
            (const $ Just ())
            (const Nothing)
            (\_ -> const Nothing)
        )
        startOrEditE
  showLoadingD <-
    toggle False $
      fmapMaybe
        ( these
            (const Nothing)
            (const (Just ()))
            (\_ -> const (Just ()))
        )
        startOrEditE
  startOrEditEE <- dyn $ ffor2 showPhraseListD showLoadingD $ \showPhraseList showLoading ->
    if showLoading
      then never <$ loading
      else
        if showPhraseList
          then do
            currentPhrases <- sample $ current phraseD
            elAttr "p" ("class" =: "title") $ text "Add some phrases then click save"
            phraseListD <- el "div" $ phraseList currentPhrases
            el "br" blank
            saveClickE <-
              tagPromptlyDyn phraseListD . domEvent Click . fst
                <$$> elAttr' "button" ("class" =: "button is-success")
                $ text "Save"
            pure $ (This . Just <$> saveClickE)
          else do
            elAttr "div" ("class" =: "content") $ do
              elAttr "h2" ("class" =: "is-large") $ dynText $ ffor totalPhrasesSubmittedD $ \t -> (T.pack $ show t) <> " phrase(s) submitted so far."
            editPhraseListClickE <-
              domEvent Click . fst <$$> elAttr' "button" ("class" =: "button is-success") $
                text "Edit Phrases"
            el "hr" blank
            startGameClickE <-
              domEvent Click . fst <$$> elAttr' "button" ("class" =: "button is-danger") $
                text "Start Game"
            pure $ align (Nothing <$ editPhraseListClickE) startGameClickE
  startOrEditE <- switchHold never startOrEditEE
  phraseD <- holdDyn initPhrases $ fmapMaybe (these id (const Nothing) (\a _ -> a)) startOrEditE
  pure $
    fmapMaybe
      ( these
          (fmap SavePhrases)
          (const (Just StartGame))
          (\_ -> const (Just StartGame))
      )
      startOrEditE

phraseList ::
  forall m t.
  ( DomBuilder t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m,
    PerformEvent t m,
    MonadJSM (Performable m),
    GhcjsDomSpace ~ DomBuilderSpace m,
    TriggerEvent t m
  ) =>
  [Text] ->
  m (Dynamic t [Text])
phraseList initPhrases = mdo
  let initPhrasesMap =
        Map.fromList $ zip [0 ..] $
          if null initPhrases then [""] else initPhrases
      phraseListDeletE = switchDyn $ leftmost . Map.elems . fmap (view _2) <$> phraseListInputsD
      phraseListAddE = domEvent Click addPhraseButton
      onPhraseListMod (Left _) xs =
        let x = maybe (-1 :: Int) fst (Map.lookupMax xs)
         in Map.insert (succ x) "" xs
      onPhraseListMod (Right i) xs = Map.delete i xs

  phraseListD <-
    foldDyn onPhraseListMod initPhrasesMap $
      leftmost [Left <$> phraseListAddE, Right <$> phraseListDeletE]
  phraseListInputsD <- elAttr "div" ("class" =: "content is-large")
    $ el "ol"
    $ listWithKey phraseListD
    $ \i v ->
      el "li" $ elAttr "div" ("class" =: "field has-addons") $ do
        initialVal <- sample $ current v
        inputEl <-
          elAttr "div" ("class" =: "control is-expanded") $
            inputElement
              ( def
                  & inputElementConfig_initialValue .~ initialVal
                  & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: "input")
              )
        removeClickE <-
          elAttr "div" ("class" =: "control") $
            (i <$$ (domEvent Click . fst <$$> elAttr' "button" ("class" =: "button is-danger") $ text "X"))
        pure (inputEl, removeClickE)
  (addPhraseButton, _) <-
    elAttr' "button" ("class" =: "button") $
      text "Add another phrase"

  -- UGG, delay to let dom catch up...
  phraseListOnAddE <- attachPromptlyDynWithMaybe (\a _ -> Map.lookupMax a) phraseListInputsD <$> delay 0.1 phraseListAddE

  performEvent_
    $ ffor phraseListOnAddE
    $ \(_, (inputEl, _)) -> liftJSM $ focus $ _inputElement_raw inputEl

  pure $ join $ mconcat . fmap (fmap pure) . Map.elems . fmap (_inputElement_value . view _1) <$> phraseListInputsD
