module Apotheka.Component.HTML.Utils
  ( _class
  , maybeElem
  , safeHref
  , whenElem
  ) where

import Prelude

import Apotheka.Data.Route (Route, routeCodec)
import Data.Maybe (Maybe(..))
import Halogen.HTML (ClassName(ClassName), HTML, IProp(..), text)
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

_class :: forall r i. String -> IProp ( class :: String | r ) i
_class = class_ <<< ClassName

safeHref :: forall r i. Route -> IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

maybeElem :: forall p i a. Maybe a -> (a -> HTML p i) -> HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = text ""

whenElem :: forall p i. Boolean -> (Unit -> HTML p i) -> HTML p i
whenElem cond f = if cond then f unit else text ""
