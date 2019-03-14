module Apotheka.Page.Home
  ( Query
  , component
  ) where

import Prelude hiding (div)

import Apotheka.Capability.Navigate (class Navigate, navigate)
import Apotheka.Component.HTML.RwFooter (viewRwFooter)
import Apotheka.Component.HTML.RwHeader (viewRwHeader)
import Apotheka.Component.HTML.Utils (_class, whenElem)
import Apotheka.Data.Profile (Profile)
import Apotheka.Data.Route (Route(Home))
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen (Component, ComponentHTML, ComponentDSL, action, lifecycleComponent)
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
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Navigate m
  => Component HH.HTML Query Unit Void m
component =
  lifecycleComponent
    { initialState: const { currentUser: Nothing }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ action Initialize
    , finalizer: Nothing
    }
  where

  eval :: Query ~> ComponentDSL State Query Void m
  eval (Initialize next) = do
    navigate Home
    pure next

  render :: State -> ComponentHTML Query
  render state@{ currentUser } =
    div_
    [ viewRwHeader currentUser Home
    , div
      [ _class "home-page" ]
      [ whenElem (isNothing currentUser) \_ -> banner
      , div
        [ _class "container page" ]
        [ div
          [ _class "row" ]
          [ mainView state
          , div
            [ _class "col-md-3" ]
            [ div
              [ _class "sidebar" ]
              [ p_ [ text "Popular Tags" ]]
            ]
          ]
        ]
      ]
    , viewRwFooter
    ]

  mainView :: forall i. State -> H.HTML i Query
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
