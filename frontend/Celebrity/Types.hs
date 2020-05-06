{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Celebrity.Types where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State.Strict (execState)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), decodeStrict, encode)
import Data.Array.IO
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import GHC.Generics (Generic)
import Language.Javascript.JSaddle
import Reflex.Firebase
import System.Random

toJSValAesonText :: ToJSON a => a -> JSM JSVal
toJSValAesonText = toJSVal . E.decodeUtf8 . BL.toStrict . encode

fromJSValAesonText :: FromJSON a => JSVal -> JSM (Maybe a)
fromJSValAesonText = fmap (join . (fmap (decodeStrict . E.encodeUtf8))) . fromJSVal

newtype Player = Player {unPlayer :: Text}
  deriving (Show, Ord, Eq, Generic)

makeLenses ''Player

instance ToJSON Player

instance FromJSON Player

instance ToJSVal Player where
  toJSVal = toJSValAesonText

instance FromJSVal Player where
  fromJSVal = fromJSValAesonText

instance ToJSString Player where
  toJSString (Player p) = toJSString p

instance FromJSString Player where
  fromJSString = Player . fromJSString

data CelebrityState = CelebrityState
  { _celebrityStateTeam1Score :: Int,
    _celebrityStateTeam2Score :: Int,
    _celebrityStateCurrentTeam :: Int,
    _celebrityStateRoundNum :: Int,
    _celebrityStateFreePhrases :: Set Text,
    _celebrityStateUsedPhrases :: Set Text,
    _celebrityStateSecondsPerRound :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''CelebrityState

instance ToJSON CelebrityState

instance FromJSON CelebrityState

instance ToJSVal CelebrityState where
  toJSVal = toJSValAesonText

instance FromJSVal CelebrityState where
  fromJSVal = fromJSValAesonText

data BetweenRoundState = BetweenRoundState
  { _betweenRoundStateCelebrityState :: CelebrityState
  }
  deriving (Show, Eq, Generic)

makeLenses ''BetweenRoundState

instance ToJSON BetweenRoundState

instance FromJSON BetweenRoundState

instance ToJSVal BetweenRoundState where
  toJSVal = toJSValAesonText

instance FromJSVal BetweenRoundState where
  fromJSVal = fromJSValAesonText

data InRoundState = InRoundState
  { _inRoundStateCurrentPlayer :: Player,
    _inRoundStateCelebrityState :: CelebrityState
  }
  deriving (Show, Eq, Generic)

makeLenses ''InRoundState

instance ToJSON InRoundState

instance FromJSON InRoundState

instance ToJSVal InRoundState where
  toJSVal = toJSValAesonText

instance FromJSVal InRoundState where
  fromJSVal = fromJSValAesonText

data WritingQuestionState = WritingQuestionState
  { _writingQuestionStatePlayersPhrases :: Map Player (NonEmpty Text),
    _writingQuestionStateSecondsPerRound :: Int
  }
  deriving (Show, Eq, Generic)

makeLenses ''WritingQuestionState

instance ToJSON WritingQuestionState

instance FromJSON WritingQuestionState

instance ToJSVal WritingQuestionState where
  toJSVal = toJSValAesonText

instance FromJSVal WritingQuestionState where
  fromJSVal = fromJSValAesonText

data GameOverState = GameOverState
  { _gameOverStateCelebrityState :: CelebrityState
  }
  deriving (Show, Eq, Generic)

makeLenses ''GameOverState

instance ToJSON GameOverState

instance FromJSON GameOverState

instance ToJSVal GameOverState where
  toJSVal = toJSValAesonText

instance FromJSVal GameOverState where
  fromJSVal = fromJSValAesonText

data GameState
  = WritingQuestions WritingQuestionState
  | BetweenRound BetweenRoundState
  | InRound InRoundState
  | GameOver GameOverState
  deriving (Show, Eq, Generic)

makePrisms ''GameState

instance ToJSON GameState

instance FromJSON GameState

instance ToJSONKey Player

instance FromJSONKey Player

instance ToJSVal GameState where
  toJSVal = toJSValAesonText

instance FromJSVal GameState where
  fromJSVal = fromJSValAesonText

initialState :: JSM FireBaseState
initialState = do
  _fireBaseStateRevision <- Revision <$> randText
  pure $
    FireBaseState
      { _fireBaseStateState =
          WritingQuestions
            WritingQuestionState
              { _writingQuestionStatePlayersPhrases = mempty,
                _writingQuestionStateSecondsPerRound = 60
              },
        _fireBaseStateRevision
      }

data FireBaseState = FireBaseState
  { _fireBaseStateState :: GameState,
    _fireBaseStateRevision :: Revision
  }
  deriving (Generic, Eq, Show)

makeLenses ''FireBaseState

instance HasRevision FireBaseState where
  getRevision FireBaseState {_fireBaseStateRevision} = _fireBaseStateRevision
  setRevision t r = t {_fireBaseStateRevision = r}

instance ToJSVal FireBaseState

instance FromJSVal FireBaseState

data R a = R1

instance Route R FireBaseState where
  renderRoute R1 = "test"

instance Route R (Id, FireBaseState) where
  renderRoute R1 = "test"

data WritingQuestionsEvent
  = SavePhrases [Text]
  | SetSecondsPerRound Int
  | StartGame

handleWritingQuestionsEvent ::
  Player ->
  WritingQuestionsEvent ->
  GameState ->
  GameState
handleWritingQuestionsEvent p wqe gs = case wqe of
  SavePhrases wl ->
    gs
      & _WritingQuestions
      . writingQuestionStatePlayersPhrases
      . at p
      .~ NE.nonEmpty (filter (not . T.null) wl)
  SetSecondsPerRound s ->
    gs
      & _WritingQuestions
      . writingQuestionStateSecondsPerRound
      .~ s
  StartGame ->
    BetweenRound $
      BetweenRoundState
        { _betweenRoundStateCelebrityState =
            CelebrityState
              { _celebrityStateTeam1Score = 0,
                _celebrityStateTeam2Score = 0,
                _celebrityStateCurrentTeam = 1,
                _celebrityStateRoundNum = 1,
                _celebrityStateFreePhrases =
                  foldMap (Set.fromList . NE.toList) $
                    gs ^. _WritingQuestions . writingQuestionStatePlayersPhrases,
                _celebrityStateUsedPhrases = mempty,
                _celebrityStateSecondsPerRound = fromMaybe 60 $ gs ^? _WritingQuestions . writingQuestionStateSecondsPerRound
              }
        }

data BetweenRoundEvent = MyTurn

handleBetweenRoundEvent :: Player -> BetweenRoundEvent -> GameState -> GameState
handleBetweenRoundEvent p bre gs = case bre of
  MyTurn ->
    InRound $
      InRoundState
        { _inRoundStateCurrentPlayer = p,
          _inRoundStateCelebrityState = fromJust $ gs ^? _BetweenRound . betweenRoundStateCelebrityState
        }

data InRoundEvent = CorrectGuesses [Text]
  deriving (Show)

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do
  b <- mb
  when b thing

handleInRoundEvent :: InRoundEvent -> GameState -> GameState
handleInRoundEvent ire gs = case ire of
  CorrectGuesses wds ->
    let newCelebState = flip
          execState
          (fromJust $ gs ^? _InRound . inRoundStateCelebrityState)
          $ do
            scoreL <- (use celebrityStateCurrentTeam) <&> \team ->
              if team == 1
                then celebrityStateTeam1Score
                else celebrityStateTeam2Score
            scoreL += length wds
            celebrityStateCurrentTeam %= \t -> 1 + (t `mod` 2)
            celebrityStateFreePhrases %= \s -> Set.difference s (Set.fromList wds)
            celebrityStateUsedPhrases %= Set.union (Set.fromList wds)
            whenM (Set.null <$> use celebrityStateFreePhrases) $ do
              celebrityStateRoundNum += 1
              use celebrityStateUsedPhrases >>= assign celebrityStateFreePhrases
              celebrityStateUsedPhrases .= mempty
     in if newCelebState ^. celebrityStateRoundNum < 4
          then BetweenRound $ BetweenRoundState {_betweenRoundStateCelebrityState = newCelebState}
          else GameOver $ GameOverState {_gameOverStateCelebrityState = newCelebState}

shuffle :: forall a. [a] -> IO [a]
shuffle xs = do
  ar <- newListArray @IOArray (1, n) xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
