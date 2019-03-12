module Apotheka.Capability.RequestArchive
  ( class RequestArchive
  , requestArchive
  ) where

import Prelude

import Apotheka.Data.Archive (Archive)
import Apotheka.Data.WrappedDate (WrappedDate)
import Data.Maybe (Maybe)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

class Monad m <= RequestArchive m where
  requestArchive :: m (Maybe { archive :: Archive, date :: WrappedDate })

instance requestArchiveHalogenM
  :: RequestArchive m
  => RequestArchive (HalogenM s f g p o m) where
    requestArchive = lift requestArchive
