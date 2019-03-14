module Apotheka.Capability.Navigate
  ( class Navigate
  , logout
  , navigate
  ) where

import Prelude

import Apotheka.Data.Route (Route)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

class Monad m <= Navigate m where
  logout   :: m Unit
  navigate :: Route -> m Unit

instance navigateHalogenM
  :: Navigate m
  => Navigate (HalogenM s f g p o m) where
    logout   = lift logout
    navigate = lift <<< navigate
