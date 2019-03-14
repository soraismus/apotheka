module Apotheka.Data.Route
  ( Route(Home, Login, Register)
  , routeCodec
  , slug
  , uname
  ) where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Apotheka.Data.Username (Username)
import Apotheka.Data.Username as Username
import Routing.Duplex (RouteDuplex', as, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Slug (Slug)
import Slug as Slug

data Route
  = Home
  | Login
  | Register

derive instance eqRoute      :: Eq      Route
derive instance genericRoute :: Generic Route _
derive instance ordRoute     :: Ord     Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home":     noArgs
  , "Login":    "login"    / noArgs
  , "Register": "register" / noArgs
  }

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.parse >>> note "Bad slug")

uname :: RouteDuplex' String -> RouteDuplex' Username
uname = as Username.toString (Username.parse >>> note "Bad username")
