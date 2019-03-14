module Apotheka.Component.HTML.Header
  ( viewHeader
  ) where

import Prelude

import Apotheka.Component.HTML.Utils (_class)
import Halogen (HTML)
import Halogen.HTML (a, div_, h1_, header_, text)
import Halogen.HTML.Properties (href)

viewHeader :: forall i p. Int -> HTML i p
viewHeader n =
  header_
    [ h1_
      [ text
          $  show n
          <> " Haskell Paper"
          <> if n == 1 then "" else "s"
      ]
    , a
      [ _class "sutble-link"
      , href "https://github.com/mitchellwrosen/haskell-papers"
      ]
      [ div_ [ text "contribute on GitHub" ]]
    ]
