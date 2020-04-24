{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Firebase
  ( Config (..),
    Query (..),
    QueryParam (..),
    Op (..),
    Direction (..),
    Route (..),
    Id (..),
    HasId (..),
    MonadFirebase,
    Revision (..),
    HasRevision (..),
    TransactionUpdate (..),
    randRevision,
    runFirebase,
    add,
    dynAdd,
    set,
    update,
    delete,
    query,
    subscribe,
    subscribeDoc,
    dynSubscribe,
    transactionUpdate,
  )
where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Text (Text)
import GHC.Generics hiding (R)
import Language.Javascript.JSaddle
import Reflex.Dom.Core hiding (Query)
import Prelude hiding (EQ, GT, LT)

data Config = Config
  { apiKey :: Text,
    authDomain :: Text,
    databaseURL :: Text,
    projectId :: Text,
    storageBucket :: Text,
    messagingSenderId :: Text,
    appId :: Text
  }
  deriving (Generic, ToJSVal, FromJSON)

data QueryParam
  = Where Text Op Text
  | Limit Int
  | OrderBy Text Direction

data Op
  = LT
  | LTE
  | EQ
  | GT
  | GTE

instance Show Op where
  show = \case
    LT -> "<"
    LTE -> "<="
    EQ -> "=="
    GT -> ">"
    GTE -> ">="

data Direction
  = Asc
  | Desc

instance Show Direction where
  show = \case
    Asc -> "asc"
    Desc -> "desc"

class MonadJSM m => MonadFirebase m where
  askDb :: m JSVal

instance MonadJSM m => MonadFirebase (ReaderT FirestoreInstance m) where
  askDb = asks unFirestoreInstance

runFirebase :: MonadJSM m => Firebase m r -> Config -> m r
runFirebase fb cfg = do
  firestore <- liftJSM $ do
    firebaseVal <- jsg ("firebase" :: String)
    _ <- firebaseVal ^. js1 ("initializeApp" :: String) cfg
    firestoreVal <- firebaseVal ^. js0 ("firestore" :: String)
    pure $ FirestoreInstance firestoreVal
  runReaderT fb firestore

type Firebase = ReaderT FirestoreInstance

newtype FirestoreInstance = FirestoreInstance {unFirestoreInstance :: JSVal}

newtype Id = Id {unId :: Text}
  deriving stock (Generic)
  deriving newtype (Show, ToJSVal, FromJSVal)

class HasId r where
  getId :: r -> Id

data Query q r = Query
  { query_route :: q r,
    query_params :: [QueryParam]
  }

class (ToJSVal r, FromJSVal r) => Route q r where
  renderRoute :: q r -> Text

-- READ
query ::
  (Route q r, MonadFirebase m) =>
  Query q r ->
  Firebase m [(Id, r)]
query q = do
  result <- collection q
  responseRef <- liftIO newEmptyMVar
  callback <- liftJSM $ function $ \_ _ (val : _) -> do
    result <- liftJSM $ getSnapshotData val
    liftIO $ putMVar responseRef result
  _ <- liftJSM $ result ^. (js0 ("get" :: String) . js1 ("then" :: String) callback)
  liftIO $ takeMVar responseRef

-- WRITE
add ::
  (Route q r, MonadFirebase m, MonadFirebase (Performable m), PerformEvent t m) =>
  q r ->
  Event t r ->
  m ()
add route itemE = do
  result <- collection $ Query route []
  void $ performEvent $
    fmap
      ( \item ->
          liftJSM $
            result ^. js1 ("add" :: String) item
      )
      itemE

dynAdd ::
  (MonadFirebase m, MonadFirebase (Performable m), TriggerEvent t m, PerformEvent t m, Route q r) =>
  Dynamic t (q r) ->
  Event t r ->
  m ()
dynAdd dynRoute itemE = do
  db <- askDb
  let act (route, item) callback = liftJSM $ do
        result <- db ^. js1 ("collection" :: String) (renderRoute route)
        void $ result ^. js1 ("add" :: String) item
  _ <- performEventAsync (act <$> attach (current dynRoute) itemE)
  pure ()

set ::
  (HasId r, MonadFirebase m, MonadFirebase (Performable m), PerformEvent t m, Route q r) =>
  q r ->
  Event t r ->
  m ()
set route itemE = do
  result <- collection $ Query route []
  void $ performEvent $
    fmap
      ( \item -> liftJSM $ do
          document <- result ^. js1 ("doc" :: String) (getId item)
          document ^. js1 ("set" :: String) item
      )
      itemE

update ::
  (HasId r, MonadFirebase m, MonadFirebase (Performable m), PerformEvent t m, Route q r) =>
  q r ->
  Event t r ->
  m ()
update route itemE = do
  result <- collection $ Query route []
  void $ performEvent $
    fmap
      ( \item -> liftJSM $ do
          document <- result ^. js1 ("doc" :: String) (getId item)
          document ^. js1 ("update" :: String) item
      )
      itemE

delete ::
  (HasId r, MonadFirebase m, MonadFirebase (Performable m), PerformEvent t m, Route q r) =>
  q r ->
  Event t r ->
  m ()
delete route itemE = do
  result <- collection $ Query route []
  void $ performEvent $ ffor itemE $ \item -> liftJSM $ do
    document <- result ^. js1 ("doc" :: String) (getId item)
    document ^. js0 ("delete" :: String)

data TransactionState
  = TransLoading
  | TransSuccess Revision
  | TransStaleFailure
  | TransFailure Text
  deriving (Eq, Show)

newtype Revision = Revision {unRevision :: Text}
  deriving (Generic, Show, Eq)

instance ToJSVal Revision

instance FromJSVal Revision

class HasRevision r where
  getRevision :: r -> Revision
  setRevision :: r -> Revision -> r

randRevision :: JSM Revision
randRevision = eval @String "window.crypto.getRandomValues(new Uint32Array(1))[0].toString()" >>= fromJSValUnchecked

data TransactionUpdate r
  = OnlyCurrent r
  | HandleStale (r -> Maybe r)

transactionUpdate ::
  forall q r t m.
  ( Route q r,
    HasRevision r,
    MonadFirebase m,
    MonadFirebase (Performable m),
    TriggerEvent t m,
    PerformEvent t m,
    MonadHold t m
  ) =>
  q r ->
  Event t (Id, TransactionUpdate r) ->
  m (Dynamic t TransactionState)
transactionUpdate route transWriteE = do
  result <- collection $ Query route []
  db <- askDb
  let act (i, transWrite) transStateCb = liftJSM $ do
        documentRef <- liftJSM $ result ^. js1 ("doc" :: String) i
        let getCb transaction = fun $ \_ _ [returnCb, doc] -> do
              currentItemStored <- doc ^. js0 @String "data" >>= fromJSValUnchecked @r
              newRevision <- randRevision
              case transWrite of
                OnlyCurrent item -> do
                  if getRevision currentItemStored == getRevision item
                    then do
                      void $ transaction ^. js2 @String "set" documentRef (setRevision item newRevision)
                      void $ call returnCb global [newRevision]
                    else void $ call returnCb global [jsNull]
                HandleStale updater -> case updater currentItemStored of
                  Nothing -> void $ call returnCb global [jsNull]
                  Just newItem -> do
                    void $ transaction ^. js2 @String "set" documentRef (setRevision newItem newRevision)
                    void $ call returnCb global [newRevision]
        let runTransCb = fun $ \_ _ [returnCb, transaction] -> do
              getP <-
                transaction
                  ^. ( js1 @String "get" documentRef
                         . js1 @String "then" (withReturnCallback (getCb transaction))
                     )
              void $ call returnCb global [getP]
        void $
          db
            ^. ( js1 @String "runTransaction" (withReturnCallback runTransCb)
                   . js1 @String
                     "then"
                     ( fun $ \_ _ [rev] -> do
                         n <- ghcjsPure $ isNull rev
                         if n
                           then liftIO $ transStateCb TransStaleFailure
                           else fromJSValUnchecked rev >>= liftIO . transStateCb . TransSuccess
                     )
                   . js1 @String "catch" (fun $ \_ _ [e] -> valToText e >>= liftIO . transStateCb . TransFailure)
               )
  resultE <- performEventAsync (act <$> transWriteE)
  holdDyn TransLoading resultE

-- Wrap a function call with an extra first parameter as a callback
-- Can use that callback to "return" from a function
withReturnCallback :: JSCallAsFunction -> JSM JSVal
withReturnCallback f = call (eval @String "f=>g=>{let a;f(p=>a=p,g);return a;}") global [f]

-- META READ
subscribeDoc ::
  forall q r t m.
  ( MonadFirebase m,
    MonadFirebase (Performable m),
    PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    Route q r,
    MonadHold t m
  ) =>
  q r ->
  Id ->
  m (Dynamic t (Maybe r))
subscribeDoc route i = do
  postBuild <- getPostBuild
  let act callback = do
        result <- collection $ Query route []
        liftJSM $ do
          jsCallback <- asyncFunction $ \_ _ [val] -> do
            dataResult <- val ^. js0 ("data" :: String) >>= fromJSValUnchecked
            liftIO $ callback (Just dataResult)
          void $ result ^. (js1 @String "doc" i . js1 @String "onSnapshot" jsCallback)
  resultE <- performEventAsync (act <$ postBuild)
  holdDyn Nothing resultE

subscribe ::
  ( MonadFirebase m,
    MonadFirebase (Performable m),
    PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    Route q r,
    MonadHold t m
  ) =>
  Query q r ->
  m (Dynamic t [(Id, r)])
subscribe q = do
  postBuild <- getPostBuild
  let act callback = do
        result <- collection q
        liftJSM $ do
          jsCallback <- asyncFunction $ \_ _ (val : _) -> do
            result <- liftJSM $ getSnapshotData val
            liftIO $ callback result
          void $ result ^. js1 ("onSnapshot" :: String) jsCallback
  resultE <- performEventAsync (act <$ postBuild)
  holdDyn [] resultE

dynSubscribe ::
  ( Route q r,
    MonadFirebase m,
    MonadFirebase (Performable m),
    TriggerEvent t m,
    PerformEvent t m,
    MonadHold t m
  ) =>
  Dynamic t (Query q r) ->
  m (Dynamic t [(Id, r)])
dynSubscribe dynQuery = do
  db <- askDb
  let act q callback = liftJSM $ do
        baseCollection <- db ^. js1 ("collection" :: String) (renderRoute $ query_route q)
        result <- applyParams (query_params q) baseCollection
        jsCallback <- asyncFunction $ \_ _ (val : _) -> do
          resultData <- getSnapshotData val
          liftIO $ callback resultData
        void $ result ^. js1 ("onSnapshot" :: String) jsCallback
  resultE <- performEventAsync (act <$> updated dynQuery)
  holdDyn [] resultE

collection ::
  (MonadFirebase m, Route q r) =>
  Query q r ->
  m JSVal
collection q = do
  db <- askDb
  liftJSM $ do
    baseCollection <- db ^. js1 ("collection" :: String) (renderRoute $ query_route q)
    applyParams (query_params q) baseCollection

dynCollection ::
  ( Route q r,
    PerformEvent t m,
    MonadHold t m,
    MonadFirebase m,
    MonadFirebase (Performable m)
  ) =>
  Dynamic t (Query q r) ->
  m (Dynamic t JSVal)
dynCollection dynQuery = do
  initialQuery <- sample $ current dynQuery
  initialResult <- collection initialQuery
  updateE <- performEvent $ collection <$> updated dynQuery
  holdDyn initialResult updateE

applyParams :: [QueryParam] -> JSVal -> JSM JSVal
applyParams q c =
  foldM applyParam c q
  where
    applyParam c = \case
      Where left op right ->
        c ^. js3 ("where" :: String) left (show op) right
      Limit count ->
        c ^. js1 ("limit" :: String) count
      OrderBy field direction ->
        c ^. js2 ("orderBy" :: String) field (show direction)

getSnapshotData :: (MonadJSM m, FromJSVal r) => JSVal -> m [(Id, r)]
getSnapshotData val = liftJSM $ do
  snapshotObj <- makeObject val
  docs <- getProp "docs" snapshotObj >>= fromJSValUnchecked
  liftJSM $
    traverse
      ( \doc -> do
          idResult <- doc ^. js ("id" :: String) >>= fromJSValUnchecked
          dataResult <- doc ^. js0 ("data" :: String) >>= fromJSValUnchecked
          pure (Id idResult, dataResult)
      )
      (docs :: [JSVal])

consoleLog val =
  void $ jsg ("console" :: String) ^. js1 ("log" :: String) val
