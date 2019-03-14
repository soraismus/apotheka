module Apotheka.Component.HTML.PaperOfTheDay
  ( viewPaperOfTheDay
  ) where

import Prelude

import Apotheka.Component.HTML.Paper (viewPaper)
import Apotheka.Component.HTML.Utils (_class, maybeElem)
import Apotheka.Data.Author (Author)
import Apotheka.Data.Paper (Paper)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Tuple (uncurry)
import Halogen (HTML)
import Halogen.HTML (div, div_, h3_, text)
import Halogen.HTML.Properties (href)

viewPaperOfTheDay
  :: forall i p
   . (forall a. Author -> a -> p a)
  -> Maybe Paper
  -> HTML i p
viewPaperOfTheDay mkQuery maybePaper =
  maybe
   (text "")
   (_viewPaperOfTheDay mkQuery)
   maybePaper

_viewPaperOfTheDay
  :: forall i p
   . (forall a. Author -> a -> p a)
  -> Paper
  -> HTML i p
_viewPaperOfTheDay mkQuery paper = do
  div_
    [ h3_ [ text "Paper of the Day" ]
    , uncurry div $ viewPaper mkQuery Nothing Nothing paper
    ]
