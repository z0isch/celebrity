{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Router
  ( -- == High-level routers
    route,
    route',
    partialPathRoute,
    -- = Low-level URL bar access
    getLoc,
    getURI,
    getUrlText,
    uriOrigin,
    URI,
    -- = History movement
    goForward,
    goBack,
  )
where

------------------------------------------------------------------------------

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Fix (MonadFix)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHCJS.Foreign (isFunction)
import GHCJS.Marshal.Pure (pFromJSVal)
import JSDOM (currentDocumentUnchecked, currentWindowUnchecked)
import JSDOM.Document (createEvent)
import JSDOM.Event (initEvent)
import JSDOM.EventM (on)
import JSDOM.EventTarget (dispatchEvent_)
import JSDOM.Generated.WindowEventHandlers (popState)
import JSDOM.History (History, back, forward, pushState)
import JSDOM.Location (getHref)
import JSDOM.PopStateEvent
import qualified JSDOM.Types as DOM
import JSDOM.Types (Location (..), PopStateEvent (..))
import JSDOM.Types (MonadJSM, uncheckedCastTo)
import JSDOM.Window (getHistory, getLocation)
import qualified Language.Javascript.JSaddle as JS
import Language.Javascript.JSaddle (JSM, Object (..), ghcjsPure, liftJSM)
import Reflex.Dom.Core hiding (EventName, Window)
import qualified URI.ByteString as U

------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-- | Manipulate and track the URL 'GHCJS.DOM.Types.Location' for dynamic
--   routing of a widget
--   These sources of URL-bar change will be reflected in the output URI
--     - Input events to 'route'
--     - Browser Forward/Back button clicks
--     - forward/back javascript calls (or 'goForward'/'goBack') Haskell calls
--     - Any URL changes followed by a popState event
--   But external calls to pushState that don't manually fire a popState
--   won't be detected
route ::
  forall t m.
  ( MonadHold t m,
    PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    HasJSContext m,
    HasJSContext (Performable m),
    MonadJSM m,
    MonadJSM (Performable m)
  ) =>
  Event t T.Text ->
  m (Dynamic t (U.URIRef U.Absolute))
route pushTo = do
  loc0 <- getURI

  _ <- performEvent $ ffor pushTo $ \t -> do
    let newState = Just t
    withHistory $ \h -> pushState h (0 :: Double) ("" :: T.Text) (newState :: Maybe T.Text)
    liftJSM dispatchEvent'

  locUpdates <- getPopState
  holdDyn loc0 locUpdates

route' ::
  forall t m a b.
  ( MonadHold t m,
    PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    HasJSContext m,
    HasJSContext (Performable m),
    MonadJSM m,
    MonadJSM (Performable m),
    MonadFix m
  ) =>
  (URI -> a -> URI) ->
  (URI -> b) ->
  Event t a ->
  m (Dynamic t b)
route' encode decode routeUpdate = do
  rec rUri <- route (T.decodeUtf8 . U.serializeURIRef' <$> urlUpdates)
      let urlUpdates = attachWith encode (current rUri) routeUpdate
  return $ decode <$> rUri

-------------------------------------------------------------------------------

-- | Route a single page app according to the part of the path after
--   pathBase
partialPathRoute ::
  forall t m.
  ( MonadHold t m,
    PostBuild t m,
    DomBuilder t m,
    TriggerEvent t m,
    PerformEvent t m,
    HasJSContext m,
    HasJSContext (Performable m),
    MonadJSM m,
    MonadJSM (Performable m),
    MonadFix m
  ) =>
  -- | The path segments not related to SPA routing
  --   (leading '/' will be added automaticaly)
  T.Text ->
  -- | Updates to the path segments used for routing
  --   These values will be appended to the base path
  Event t T.Text ->
  -- | Path segments used for routing
  m (Dynamic t [T.Text])
partialPathRoute pathBase pathUpdates = do
  route' (flip updateUrl) parseParts pathUpdates
  where
    toPath :: T.Text -> BS.ByteString
    toPath dynpath =
      T.encodeUtf8 $
        "/" <> cleanT pathBase
          <> "/"
          <> cleanT dynpath
    updateUrl :: T.Text -> URI -> URI
    updateUrl updateParts u = u & U.pathL .~ toPath updateParts
    parseParts :: URI -> [T.Text]
    parseParts u =
      maybe
        (error $ pfxErr u pathBase)
        (T.splitOn "/" . T.decodeUtf8 . cleanB)
        . BS.stripPrefix (T.encodeUtf8 $ cleanT pathBase)
        $ cleanB (u ^. U.pathL)
    cleanT = T.dropWhile (== '/')
    cleanB = BS.dropWhile (== '/')

-------------------------------------------------------------------------------
uriOrigin :: U.URIRef U.Absolute -> T.Text
uriOrigin r = T.decodeUtf8 $ U.serializeURIRef' r'
  where
    r' =
      r
        { U.uriPath = mempty,
          U.uriQuery = mempty,
          U.uriFragment = mempty
        }

-------------------------------------------------------------------------------
getPopState ::
  forall t m.
  ( MonadHold t m,
    TriggerEvent t m,
    MonadJSM m
  ) =>
  m (Event t URI)
getPopState = do
  window <- currentWindowUnchecked
  wrapDomEventMaybe window (`on` popState) $ do
    loc <- getLocation window
    locStr <- getHref loc
    return . hush $ U.parseURI U.laxURIParserOptions (T.encodeUtf8 locStr)

-------------------------------------------------------------------------------
goForward :: (HasJSContext m, MonadJSM m) => m ()
goForward = withHistory forward

-------------------------------------------------------------------------------
goBack :: (HasJSContext m, MonadJSM m) => m ()
goBack = withHistory back

-------------------------------------------------------------------------------
withHistory :: (HasJSContext m, MonadJSM m) => (History -> m a) -> m a
withHistory act = do
  w <- currentWindowUnchecked
  h <- getHistory w
  act h

-------------------------------------------------------------------------------

-- | (Unsafely) get the 'GHCJS.DOM.Location.Location' of a window
getLoc :: (HasJSContext m, MonadJSM m) => m Location
getLoc = do
  win <- currentWindowUnchecked
  loc <- getLocation win
  return loc

-------------------------------------------------------------------------------

-- | (Unsafely) get the URL text of a window
getUrlText :: (HasJSContext m, MonadJSM m) => m T.Text
getUrlText = getLoc >>= getHref

-------------------------------------------------------------------------------
type URI = U.URIRef U.Absolute

-------------------------------------------------------------------------------
getURI :: (HasJSContext m, MonadJSM m) => m URI
getURI = do
  l <- getUrlText
  return
    $ either (error "No parse of window location") id
      . U.parseURI U.laxURIParserOptions
    $ T.encodeUtf8 l

dispatchEvent' :: JSM ()
dispatchEvent' = do
  window <- currentWindowUnchecked
  obj@(Object o) <- JS.create
  JS.objSetPropertyByName obj ("cancelable" :: Text) True
  JS.objSetPropertyByName obj ("bubbles" :: Text) True
  JS.objSetPropertyByName obj ("view" :: Text) window
  event <- JS.jsg ("PopStateEvent" :: Text) >>= ghcjsPure . isFunction >>= \case
    True -> newPopStateEvent ("popstate" :: Text) $ Just $ pFromJSVal o
    False -> do
      doc <- currentDocumentUnchecked
      event <- createEvent doc ("PopStateEvent" :: Text)
      initEvent event ("popstate" :: Text) True True
      JS.objSetPropertyByName obj ("view" :: Text) window
      return $ uncheckedCastTo PopStateEvent event

  dispatchEvent_ window event

-------------------------------------------------------------------------------
hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

-------------------------------------------------------------------------------
pfxErr :: URI -> T.Text -> String
pfxErr pn pathBase =
  T.unpack $
    "Encountered path (" <> T.decodeUtf8 (U.serializeURIRef' pn)
      <> ") without expected prefix ("
      <> pathBase
      <> ")"
