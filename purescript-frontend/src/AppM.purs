module Apotheka.AppM where

import Prelude

import Control.Monad.Reader.Trans
  ( class MonadAsk
  , ReaderT
  , ask
  , asks
  , runReaderT
  )

import Apotheka.Api.Endpoint (Endpoint(..))
import Apotheka.Api.Request (BaseURL, RequestMethod(..))
import Apotheka.Api.Utils (decode, mkRequest)
import Apotheka.Capability.LogMessages (class LogMessages)
import Apotheka.Capability.Now (class Now)
import Apotheka.Capability.RequestArchive (class RequestArchive)
import Apotheka.Data.Archive (decodeArchive)
import Apotheka.Data.Log as Log
import Apotheka.Data.WrappedDate (WrappedDate(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Type.Equality (class TypeEquals, from)

data LogLevel = Dev | Prod

type Env = 
  { logLevel :: LogLevel 
  , baseUrl :: BaseURL
  }

derive instance eqLogLevel  :: Eq  LogLevel
derive instance ordLogLevel :: Ord LogLevel

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM     :: Functor     AppM
derive newtype instance applyAppM       :: Apply       AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM        :: Bind        AppM
derive newtype instance monadAppM       :: Monad       AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM    :: MonadAff    AppM

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do 
    env <- ask
    liftEffect case env.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance requestArchiveAppM :: RequestArchive AppM where
  requestArchive = do
    maybeJson    <- mkRequest { endpoint: ArchiveEndpoint, method: Get }
    maybeArchive <- decode decodeArchive maybeJson
    date         <- liftEffect Now.nowDate
    let mkResponse archive = { archive, date: WrappedDate date }
    pure (map mkResponse maybeArchive)
