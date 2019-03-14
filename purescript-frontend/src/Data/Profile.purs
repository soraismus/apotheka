module Apotheka.Data.Profile
  ( Profile
  ) where

import Prelude

import Apotheka.Data.Avatar (Avatar)
import Apotheka.Data.Email (Email)
import Apotheka.Data.Username (Username)
import Data.Maybe (Maybe)

type Profile =
  { bio      :: Maybe String
  , email    :: Email
  , image    :: Maybe Avatar
  , username :: Username
  }
