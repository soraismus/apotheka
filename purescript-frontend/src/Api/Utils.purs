module Apotheka.Api.Utils
  ( authenticate
  , decode
  , decodeAt
  , decodeWithAt
  , mkAuthRequest
  , mkRequest
  ) where

import Prelude

import Affjax (request)
import Apotheka.Api.Request
  ( BaseURL
  , RequestOptions
  , Token
  , formJsonRequest
  , readToken
  , writeToken
  )
import Apotheka.Capability.LogMessages (class LogMessages, logError)
import Apotheka.Capability.Now (class Now)
import Apotheka.Data.Profile (Profile)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(Left, Right), hush)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

authenticate
  :: forall m a r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL, currentUser :: Ref (Maybe Profile) | r } m
  => LogMessages m
  => Now m
  => (BaseURL -> a -> m (Either String (Tuple Token Profile)))
  -> a
  -> m (Maybe Profile)
authenticate f x = do
  { baseUrl, currentUser } <- ask
  f baseUrl x >>= case _ of
    Left err ->
      logError err *> pure Nothing
    Right (Tuple token profile) -> do
      liftEffect $ writeToken token
      liftEffect $ Ref.write (Just profile) currentUser
      pure (Just profile)

decode
  :: forall m a
   . LogMessages m
  => Now m
  => (Json -> Either String a)
  -> Maybe Json
  -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing
decode decoder (Just json) = case decoder json of
  Left err -> logError err *> pure Nothing
  Right response -> pure (Just response)

decodeAt :: forall a. DecodeJson a => String -> Json -> Either String a
decodeAt = decodeWithAt decodeJson

decodeWithAt
  :: forall a
   . DecodeJson a
  => (Json -> Either String a)
  -> String
  -> Json
  -> Either String a
decodeWithAt decoder key = decoder <=< (_ .: key) <=< decodeJson

mkAuthRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkAuthRequest options = liftEffect readToken >>= _mkRequest options

mkRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest options = _mkRequest options Nothing

_mkRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> Maybe Token
  -> m (Maybe Json)
_mkRequest options mToken = do
  { baseUrl } <- ask
  response <- liftAff $ request $ formJsonRequest baseUrl mToken options
  pure $ hush response.body
