module Apotheka.Component.HTML.RwFooter
  ( viewRwFooter
  ) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Apotheka.Component.HTML.Utils (_class)

viewRwFooter :: forall i p. HH.HTML i p
viewRwFooter =
  HH.footer_
    [ HH.div
      [ _class "container" ]
      [ HH.div_
        [ HH.a
          [ _class "logo-font"
          , HP.href "/"
          ]
          [ HH.text "apotheka" ]
        ]
      , HH.div
        [ _class "attribution" ]
        [ HH.text "An explorable archive of papers in mathematics and computer science" ]
      , HH.div
        [ _class "attribution" ]
        [ HH.text "Inspired by "
        , HH.a
          [ HP.href "https://thinkster.io" ]
          [ HH.text "Thinkster" ]
        , HH.text " and "
        , HH.a
          [ HP.href "https://mitchellwrosen.github.io/haskell-papers/" ]
          [ HH.text "Mitchell Wrosen's Haskell Papers" ]
        , HH.text "."
        ]
      , HH.div
        [ _class "attribution" ]
        [ HH.text "Code & design licensed under MIT. Implemented by "
        , HH.a
          [ HP.href "mailto:hilty.matthew@gmail.com?subject=apotheka" ]
          [ HH.text "Matthew Hilty" ]
        , HH.text "."
        ]
      ]
    ]
