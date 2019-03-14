module Apotheka.Capability.Resource.User
  ( class ManageUser
  , loginUser
  , registerUser
  , getCurrentUser
  ) where

import Prelude

import Apotheka.Api.Request (LoginFields, RegisterFields)
import Apotheka.Data.Profile (Profile)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageUser m where
  getCurrentUser :: m (Maybe Profile)
  loginUser      :: LoginFields -> m (Maybe Profile)
  registerUser   :: RegisterFields -> m (Maybe Profile)

instance manageUserHalogenM
  :: ManageUser m
  => ManageUser (HalogenM s f g p o m) where
    getCurrentUser = lift getCurrentUser
    loginUser      = lift <<< loginUser
    registerUser   = lift <<< registerUser
