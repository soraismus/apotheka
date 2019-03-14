module Main where

import Prelude

import Affjax (printResponseFormatError, request)
import Apotheka.Api.Request
  ( BaseURL(BaseURL)
  , RequestMethod(Get)
  , formJsonRequest
  , readToken
  )
import Apotheka.Api.Endpoint (Endpoint(User))
import Apotheka.Api.Utils (decodeAt)
import Apotheka.AppM (Env, LogLevel(Dev), runAppM)
import Apotheka.Component.Router as Router
import Apotheka.Data.Route (Route, routeCodec)
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen (Component, action, hoist)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (HTML)
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (getHash, matchesWith)

main :: Effect Unit
main = runHalogenAff do
  body        <- awaitBody
  initialHash <- liftEffect $ getHash
  currentUser <- liftEffect $ Ref.new Nothing

  let
    baseUrl :: BaseURL
    baseUrl = BaseURL "http://127.0.0.1:8080"

    logLevel :: LogLevel
    logLevel = Dev

    env :: Env
    env = { baseUrl, currentUser, logLevel }

    initialRoute :: Maybe Route
    initialRoute = hush $ parse routeCodec initialHash

    routerComponent :: Component HTML Router.Query Router.Input Void Aff
    routerComponent = hoist (runAppM env) Router.component

  halogenIO <- runUI routerComponent initialRoute body

  void $ liftEffect $ matchesWith (parse routeCodec) \mOldHash newHash ->
    unless (mOldHash == Just newHash) do
       launchAff_ $ halogenIO.query $ action $ Router.Navigate newHash

  liftEffect readToken >>= traverse_ \token -> do
    let opts = { endpoint: User, method: Get }
    response <- liftAff $ request $ formJsonRequest baseUrl (Just token) opts
    let user = decodeAt "user" =<< lmap printResponseFormatError response.body
    liftEffect $ Ref.write (hush user) currentUser
    pure unit

  pure unit
