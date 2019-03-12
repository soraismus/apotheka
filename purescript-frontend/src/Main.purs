module Main where

import Prelude

import Apotheka.Api.Request (BaseURL(..))
import Apotheka.AppM (Env, LogLevel(..), runAppM)
import Apotheka.Component.Root as Root
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  let
    environment :: Env
    environment =
      { baseUrl: BaseURL "http://127.0.0.1:8080"
      , logLevel: Dev
      }

    rootComponent :: H.Component HH.HTML Root.Query Unit Void Aff
    rootComponent = H.hoist (runAppM environment) Root.component

  runUI rootComponent unit body
