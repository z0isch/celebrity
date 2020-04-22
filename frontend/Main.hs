{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
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

data R a = R1

instance Route R Test where
  renderRoute R1 = "test"

instance Route R (Id, Test) where
  renderRoute R1 = "test"

data Test = Test {a :: Text, revision :: Revision}
  deriving (Generic, Show)

instance HasRevision Test where
  getRevision Test {revision} = revision
  setRevision t r = t {revision = r}

instance ToJSVal Test

instance FromJSVal Test

main :: IO ()
main =
  mainWidget $ do
    script1 <- fst <$> elAttr' "script" ("src" =: "https://www.gstatic.com/firebasejs/7.14.1/firebase.js") blank
    void $ widgetHold (el "pre" $ text "Loading")
      $ ffor (domEvent Load script1)
      $ \_ -> flip runFirebase firebaseConfig $ do
        pbE <- getPostBuild
        randE <- performEvent $ liftJSM randRevision <$ pbE
        fbD <- subscribe (Query @R @Test R1 [])
        (transD1, transD2) <-
          (,)
            <$> ( transactionUpdate R1
                    $ ffor randE
                    $ \r ->
                      ( Id "0Q0TW5fNUmWYWiZJA8jY",
                        OnlyCurrent $ Test "a" (Revision "3871429086")
                      )
                )
            <*> ( transactionUpdate R1
                    $ ffor randE
                    $ \r ->
                      ( Id "0Q0TW5fNUmWYWiZJA8jY",
                        OnlyCurrent $ Test "b" (Revision "3871429086")
                      )
                )
        el "pre" $ display transD1
        el "pre" $ display transD2
        el "ol" $ simpleList fbD $ \d -> el "li" $ display d
        pure ()
