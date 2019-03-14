module Apotheka.Page.Home
  ( Query
  , component
  ) where

import Prelude

import Apotheka.Capability.Navigate (class Navigate, navigate)
import Apotheka.Data.Profile (Profile)
import Apotheka.Data.Route (Route(Home))
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen
  ( Component
  , ComponentDSL
  , ComponentHTML
  , action
  , lifecycleComponent
  )
import Halogen.HTML (HTML, text)

data Query a
  = Initialize a

type State = Unit

component
  :: forall m r
   . MonadAff m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Navigate m
  => Component HTML Query Unit Void m
component =
  lifecycleComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ action Initialize
    , finalizer: Nothing
    }
  where
  render :: State -> ComponentHTML Query
  render state = text "Home"

  eval :: Query ~> ComponentDSL State Query Void m
  eval (Initialize next) = do
    navigate Home
    pure next
