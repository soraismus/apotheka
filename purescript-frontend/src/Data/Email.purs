module Apotheka.Data.Email where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

newtype Email = Email String

derive instance eqEmail      :: Eq      Email
derive instance genericEmail :: Generic Email _
derive instance newtypeEmail :: Newtype Email _
derive instance ordEmail     :: Ord     Email

derive newtype instance decodeJsonEmail :: DecodeJson Email
derive newtype instance encodeJsonEmail :: EncodeJson Email

instance showEmail :: Show Email where
  show = genericShow
