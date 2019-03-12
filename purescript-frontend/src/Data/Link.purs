module Apotheka.Data.Link
  ( Link
  , parse
  , toString
  ) where

import Prelude

import Apotheka.Data.Present (class Present)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

newtype Link = Link String

derive instance eqLink      :: Eq      Link
derive instance genericLink :: Generic Link _

derive newtype instance decodeJsonLink :: DecodeJson Link
derive newtype instance encodeJsonLink :: EncodeJson Link

instance showLink :: Show Link where
  show = genericShow

instance presentLink :: Present Link where
  present (Link str) = str

parse :: String -> Maybe Link
parse "" = Nothing
parse str = Just (Link str)

toString :: Link -> String
toString (Link str) = str
