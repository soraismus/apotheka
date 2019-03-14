module Apotheka.Component.HTML.PaperList
  ( viewPaperList
  ) where

import Prelude

import Apotheka.Component.HTML.Paper (viewPaper)
import Apotheka.Component.HTML.Utils (_class)
import Apotheka.Data.Author (Author)
import Apotheka.Data.Paper (Paper)
import Data.Maybe (Maybe(Just))
import Data.Tuple (uncurry)
import Halogen (HTML)
import Halogen.HTML (li, ul)

viewPaperList
  :: forall i p r0 r1
   . (forall a. Author -> a -> p a)
  -> { filters :: { author :: String, title :: String | r0 }
     , selectedPapers :: Array Paper
     | r1
     }
  -> HTML i p
viewPaperList mkQuery { filters:{ author, title }, selectedPapers } =
  ul
    [ _class "paper-list" ]
    (map
      (uncurry li <<< viewPaper mkQuery (Just author) (Just title))
      selectedPapers)
