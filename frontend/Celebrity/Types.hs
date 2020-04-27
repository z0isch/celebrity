{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Celebrity.Types where

import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding as E
import GHC.Generics (Generic)
import Language.Javascript.JSaddle
import Reflex.Firebase

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

data WritingQuestionState = WritingQuestionState
  { _writingQuestionStatePlayersWords :: Map Player (NonEmpty Text)
  }
  deriving (Show, Eq, Generic)

makeLenses ''WritingQuestionState

instance ToJSON WritingQuestionState

instance FromJSON WritingQuestionState

instance ToJSVal WritingQuestionState where
  toJSVal = toJSValAesonText

instance FromJSVal WritingQuestionState where
  fromJSVal = fromJSValAesonText

data GameState
  = WritingQuestions WritingQuestionState
  | WaitingForRound
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
      { _fireBaseStateState = WritingQuestions WritingQuestionState {_writingQuestionStatePlayersWords = mempty},
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
