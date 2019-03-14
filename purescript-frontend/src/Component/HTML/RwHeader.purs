module Apotheka.Component.HTML.RwHeader
  ( viewRwHeader
  ) where

import Prelude

import Apotheka.Component.HTML.Utils (_class, maybeElem, safeHref, whenElem)
import Apotheka.Data.Avatar as Avatar
import Apotheka.Data.Profile (Profile)
import Apotheka.Data.Route (Route(..))
import Apotheka.Data.Username as Username
import Data.Maybe (Maybe, isNothing, isJust)
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

viewRwHeader :: forall i p. Maybe Profile -> Route -> HH.HTML i p
viewRwHeader currentUser route =
  HH.nav
    [ _class "navbar navbar-light" ]
    [ HH.div
      [ _class "container" ]
      [ HH.a
        [ _class "navbar-brand"
        , safeHref Home
        ]
        [ HH.text "apotheka" ]
      , HH.ul
        [ _class "nav navbar-nav pull-xs-right" ]
        [ navItem Home
            [ HH.text "Home" ]
        , whenElem (isJust currentUser) \_ ->
            navItem Home
              [ HH.i
                [ _class "ion-compose" ]
                [ HH.text " New Post" ]
              ]
        , whenElem (isJust currentUser) \_ ->
            navItem Home
              [ HH.i
                [ _class "ion-gear-a" ]
                [ HH.text " Settings" ]
              ]
        , maybeElem currentUser \profile ->
            navItem Home
              [ HH.img
                [ _class "user-pic"
                , HP.src $ Avatar.toStringWithDefault profile.image
                ]
              , HH.text $ Username.toString profile.username
              ]
        , whenElem (isNothing currentUser) \_ ->
            navItem Login
              [ HH.text "Log in" ]
        , whenElem (isNothing currentUser) \_ ->
            navItem Register
              [ HH.text "Sign up" ]
        ]
      ]
    ]

  where

  navItem r html =
    HH.li
      [ _class "nav-item" ]
      [ HH.a
        [ _class $ "nav-link" <> guard (route == r) " active"
        , safeHref r
        ]
        html
      ]
