module Apotheka.Component.Router
  ( Input
  , Query(Navigate)
  , component
  ) where

import Prelude

import Apotheka.Capability.LogMessages (class LogMessages)
import Apotheka.Capability.Navigate (class Navigate)
import Apotheka.Capability.Now (class Now)
import Apotheka.Capability.Resource.User (class ManageUser)
import Apotheka.Data.Profile (Profile)
import Apotheka.Data.Route (Route(Home, Login, Register))
import Apotheka.Page.Home as Home
import Apotheka.Page.Login as Login
import Apotheka.Page.Register as Register
import Control.Monad.Reader (class MonadAsk)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH

type State = { route :: Route }

data Query a = Navigate Route a

type Input = Maybe Route

type ChildQuery = Coproduct3
  Home.Query
  Login.Query
  Register.Query

type ChildSlot = Either3
  Unit
  Unit
  Unit

component
  :: forall m r
   . MonadAff m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageUser m
  => H.Component HH.HTML Query Input Void m
component =
  H.parentComponent
    { initialState: \initialRoute -> { route: fromMaybe Home initialRoute } 
    , render
    , eval
    , receiver: const Nothing
    }

  where 

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (Navigate route' next) = do
    { route } <- H.get 
    when (route /= route') do
      H.modify_ (_ { route = route' })
    pure next

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route } = case route of
    Home -> 
      HH.slot' CP.cp1 unit Home.component unit absurd
    Login -> 
      HH.slot' CP.cp2 unit Login.component unit absurd
    Register -> 
      HH.slot' CP.cp3 unit Register.component unit absurd
