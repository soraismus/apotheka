module Apotheka.Api.Endpoint
  ( Endpoint(Archive, Login, User, Users)
  , endpointCodec
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Endpoint
  = Archive
  | Login
  | User
  | Users

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root $ sum
    { "Archive": "papers.json" / noArgs
    , "Login":   "users"       / "login" / noArgs
    , "User":    "user"        / noArgs
    , "Users":   "users"       / noArgs
    }
