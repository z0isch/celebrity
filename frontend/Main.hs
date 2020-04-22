{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Text (Text)
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

data GameState = WritingQuestions WritingQuestionState | WatingForRound | Round
  deriving (Show, Generic)

instance ToJSON GameState

instance FromJSON GameState

instance ToJSVal GameState where
  toJSVal = toJSValAesonText

instance FromJSVal GameState where
  fromJSVal = fromJSValAesonText

data WritingQuestionState = WritingQuestionState
  { _writingQuestionStatePlayers :: Map Text (NonEmpty Text)
  }
  deriving (Show, Generic)

instance ToJSON WritingQuestionState

instance FromJSON WritingQuestionState

instance ToJSVal WritingQuestionState where
  toJSVal = toJSValAesonText

instance FromJSVal WritingQuestionState where
  fromJSVal = fromJSValAesonText

toJSValAesonText :: ToJSON a => a -> JSM JSVal
toJSValAesonText = toJSVal . E.decodeUtf8 . BL.toStrict . encode

fromJSValAesonText :: FromJSON a => JSVal -> JSM (Maybe a)
fromJSValAesonText = fmap (join . (fmap (decodeStrict . E.encodeUtf8))) . fromJSVal

data FireBaseState = FireBaseState {state :: GameState, revision :: Revision}
  deriving (Generic, Show)

instance HasRevision FireBaseState where
  getRevision FireBaseState {revision} = revision
  setRevision t r = t {revision = r}

instance ToJSVal FireBaseState

instance FromJSVal FireBaseState

data R a = R1

instance Route R FireBaseState where
  renderRoute R1 = "test"

instance Route R (Id, FireBaseState) where
  renderRoute R1 = "test"

main :: IO ()
main =
  mainWidget $ do
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
                      ( Id "B3aq3VhaXBqkDvERUzZH",
                        OnlyCurrent $
                          FireBaseState
                            (WritingQuestions (WritingQuestionState ("a" =: (NE.fromList ["oh yeah"]))))
                            (Revision "2612949843")
                      )
                )
            <*> ( transactionUpdate R1
                    $ ffor randE
                    $ \r ->
                      ( Id "B3aq3VhaXBqkDvERUzZH",
                        OnlyCurrent $
                          FireBaseState
                            (WritingQuestions (WritingQuestionState ("b" =: (NE.fromList ["baby"]))))
                            (Revision "2612949843")
                      )
                )
        el "pre" $ display transD1
        el "pre" $ display transD2
        el "ol" $ simpleList fbD $ \d -> el "li" $ display d
        pure ()
