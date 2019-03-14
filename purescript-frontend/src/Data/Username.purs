module Apotheka.Data.Username 
  ( Username
  , parse
  , toString
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

newtype Username = Username String

derive instance eqUsername      :: Eq      Username
derive instance genericUsername :: Generic Username _
derive instance ordUsername     :: Ord     Username

derive newtype instance decodeJsonUsername :: DecodeJson Username
derive newtype instance encodeJsonUsername :: EncodeJson Username

instance showUsername :: Show Username where
  show = genericShow

parse :: String -> Maybe Username
parse "" = Nothing
parse str = Just (Username str)

toString :: Username -> String
toString (Username str) = str
