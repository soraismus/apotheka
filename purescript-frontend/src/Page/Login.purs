module Apotheka.Page.Login
  ( Query
  , component
  ) where

import Prelude hiding (div)

import Apotheka.Capability.Navigate (class Navigate, navigate)
import Apotheka.Capability.Resource.User (class ManageUser, loginUser)
import Apotheka.Component.HTML.RwHeader (viewRwHeader)
import Apotheka.Component.HTML.Utils (_class, safeHref)
import Apotheka.Component.Utils (guardNoSession)
import Apotheka.Data.Email (Email)
import Apotheka.Data.Profile (Profile)
import Apotheka.Data.Route (Route(..))
import Apotheka.Form.Field (submit)
import Apotheka.Form.Field as Field
import Apotheka.Form.Validation as V
import Control.Monad.Reader (class MonadAsk)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Formless as F
import Formless as Formless
import Halogen
  ( Component
  , ParentDSL
  , ParentHTML
  , action
  , lifecycleParentComponent
  )
import Halogen.HTML (HTML, h1, text, p, a, slot, div, form_, fieldset_)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = Initialize a
  | HandleForm (F.Message' LoginForm) a

type ChildQuery m = F.Query' LoginForm m

component
  :: forall m r
   . MonadAff m
  => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Navigate m
  => ManageUser m
  => Component HTML Query Unit Void m
component =
  lifecycleParentComponent
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ action Initialize
    , finalizer: Nothing
    }
  where
  eval :: Query ~> ParentDSL Unit Query (ChildQuery m) Unit Void m
  eval = case _ of
    Initialize a -> do
      guardNoSession
      pure a

    HandleForm msg a -> case msg of
      F.Submitted formOutputs -> do
        mbUser <- loginUser $ F.unwrapOutputFields formOutputs
        traverse_ (\_ -> navigate Home) mbUser
        pure a
      _ -> pure a

  render :: Unit -> ParentHTML Query (ChildQuery m) Unit m
  render _ =
    container
      [ h1
        [ _class "text-xs-center"]
        [ text "Sign In" ]
      , p
        [ _class "text-xs-center" ]
        [ a
          [ safeHref Register ]
          [ text "Need an account?" ]
        ]
      , slot unit Formless.component
          { initialInputs: F.mkInputFields formProxy
          , validators
          , render: renderFormless
          }
          (HE.input HandleForm)
      ]
    where
    container html =
      div
        [ _class "auth-page" ]
        [ viewRwHeader Nothing Login
        , div
          [ _class "container page" ]
          [ div
            [ _class "row" ]
            [ div
              [ _class "col-md-6 offset-md-3 col-xs12" ]
              html
            ]
          ]
        ]

newtype LoginForm r f = LoginForm (r
  ( email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

formProxy :: F.FormProxy LoginForm
formProxy = F.FormProxy

proxies :: F.SProxies LoginForm
proxies = F.mkSProxies formProxy

validators :: forall form m. Monad m => LoginForm Record (F.Validation form m)
validators = LoginForm
  { email: V.required >>> V.minLength 3 >>> V.emailFormat
  , password: V.required >>> V.minLength 2 >>> V.maxLength 20
  }

renderFormless
  :: forall m
   . MonadAff m
  => F.State LoginForm m
  -> F.HTML' LoginForm m
renderFormless fstate =
  form_
    [ fieldset_
      [ email
      , password
      , submit "Log in"
      ]
    ]
  where
  email =
    Field.input proxies.email fstate.form
      [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

  password =
    Field.input proxies.password fstate.form
      [ HP.placeholder "Password", HP.type_ HP.InputPassword ]
