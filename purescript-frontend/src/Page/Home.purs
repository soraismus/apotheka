module Apotheka.Page.Home
  ( Query
  , component
  ) where

import Prelude hiding (div)

import Apotheka.Capability.LogMessages (class LogMessages)
import Apotheka.Capability.Navigate (class Navigate, navigate)
import Apotheka.Capability.Now (class Now)
import Apotheka.Capability.RequestArchive (class RequestArchive)
import Apotheka.Component.HTML.RwFooter (viewRwFooter)
import Apotheka.Component.HTML.RwHeader (viewRwHeader)
import Apotheka.Component.HTML.Utils (_class)
import Apotheka.Component.Root as Root
import Apotheka.Data.Profile (Profile)
import Apotheka.Data.Route (Route(Home))
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen
  ( Component
  , action
  , lifecycleParentComponent
  )
import Halogen as H
import Halogen.HTML (div, div_, h1, p_, text)
import Halogen.HTML as HH

data Query a
  = Initialize a

type State =
  { currentUser :: Maybe Profile }

component
  :: forall m r
   . MonadAff m
  => LogMessages m
  => Now m
  => RequestArchive m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Navigate m
  => Component HH.HTML Query Unit Void m
component =
  lifecycleParentComponent
    { initialState: const { currentUser: Nothing }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ action Initialize
    , finalizer: Nothing
    }
  where

  eval :: Query ~> H.ParentDSL State Query Root.Query Unit Void m
  eval (Initialize next) = do
    navigate Home
    pure next

  render :: State -> H.ParentHTML Query Root.Query Unit m
  render state@{ currentUser } =
    div_
    [ viewRwHeader currentUser Home
    , HH.slot unit Root.component unit absurd
    , viewRwFooter
    ]

  mainView :: forall i p. State -> H.HTML i p
  mainView state =
    div
    [ _class "col-md-9" ]
    [ div
      [ _class "feed-toggle" ]
      [ text "Feed Toggle" ]
    ]

  banner :: forall i p. HH.HTML i p
  banner =
    div
    [ _class "banner" ]
    [ div
      [ _class "container" ]
      [ h1
        [ _class "logo-font" ]
        [ text "apotheka" ]
      , p_
        [ text "A place to share your knowledge." ]
      ]
    ]
