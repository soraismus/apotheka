module Apotheka.Data.Avatar 
  ( Avatar
  , parse
  , toString
  , toStringWithDefault
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

newtype Avatar = Avatar String

derive instance eqAvatar      :: Eq      Avatar
derive instance genericAvatar :: Generic Avatar _

derive newtype instance decodeJsonAvatar :: DecodeJson Avatar
derive newtype instance encodeJsonAvatar :: EncodeJson Avatar

instance showAvatar :: Show Avatar where
  show = genericShow

parse :: String -> Maybe Avatar
parse "" = Nothing
parse str = Just (Avatar str)

toString :: Avatar -> String
toString (Avatar str) = str

toStringWithDefault :: Maybe Avatar -> String
toStringWithDefault (Just av) = toString av
toStringWithDefault Nothing =
  "https://static.productionready.io/images/smiley-cyrus.jpg"
