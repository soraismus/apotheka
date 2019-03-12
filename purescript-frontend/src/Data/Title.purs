module Apotheka.Data.Title
  ( Title
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

newtype Title = Title String

derive instance eqTitle      :: Eq      Title
derive instance genericTitle :: Generic Title _

derive newtype instance encodeJsonTitle :: EncodeJson Title
derive newtype instance decodeJsonTitle :: DecodeJson Title

instance showTitle :: Show Title where
  show = genericShow

instance presentTitle :: Present Title where
  present (Title str) = str

parse :: String -> Maybe Title
parse "" = Nothing
parse str = Just (Title str)

toString :: Title -> String
toString (Title str) = str
