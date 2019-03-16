module Apotheka.Form.Field
  ( input
  , submit
  ) where

import Prelude

import Apotheka.Component.HTML.Utils (_class, maybeElem)
import Apotheka.Form.Validation (errorToString)
import Apotheka.Form.Validation as V
import DOM.HTML.Indexed (HTMLinput)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Row as Row

input
  :: forall pq cq cs form m fields inputs sym o t0 t1
   . IsSymbol sym
  => Newtype (form Record F.FormField) { | fields }
  => Newtype (form Variant F.InputFunction) (Variant inputs)
  => Row.Cons sym (F.FormField V.FormError String o) t0 fields
  => Row.Cons sym (F.InputFunction V.FormError String o) t1 inputs
  => SProxy sym
  -> form Record F.FormField
  -> Array (HH.IProp HTMLinput (F.Query pq cq cs form m Unit))
  -> F.HTML pq cq cs form m
input sym form props =
  HH.fieldset
    [ _class "form-group" ]
    [ HH.input
      ( append
          [ _class "form-control form-control-lg"
          , HP.value $ F.getInput sym form
          , HE.onValueInput $ HE.input $ F.setValidate sym
          ]
          props
      )
    , maybeElem (F.getError sym form) \err ->
        HH.div
          [ _class "error-messages" ]
          [ HH.text $ errorToString err ]
    ]

submit :: forall pq cq cs form m. String -> F.HTML pq cq cs form m
submit buttonText =
  HH.button
    [ _class "btn btn-lg btn-primary pull-xs-right"
    , HE.onClick $ HE.input_ F.submit
    ]
    [ HH.text buttonText ]
