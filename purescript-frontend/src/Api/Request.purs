module Apotheka.Api.Request
  ( BaseURL(BaseURL)
  , CredentialsRep
  , LoginFields
  , RegisterFields
  , RequestMethod(Get, Post, Put, Delete)
  , RequestOptions
  , Token
  , Unlifted
  , formJsonRequest
  , login
  , readToken
  , register
  , removeToken
  , writeToken
  ) where

import Prelude

import Affjax (Request, printResponseFormatError, request)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(RequestHeader))
import Affjax.ResponseFormat as ResponseFormat
import Apotheka.Api.Endpoint (Endpoint(Login, Users), endpointCodec)
import Apotheka.Data.Email (Email)
import Apotheka.Data.Profile (Profile)
import Apotheka.Data.Username (Username)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left))
import Data.HTTP.Method (Method(DELETE, GET, POST, PUT))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

newtype BaseURL = BaseURL String

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

newtype Token = Token String

derive instance eqToken  :: Eq  Token
derive instance ordToken :: Ord Token

type Unlifted a = a

type CredentialsRep f r =
  ( email :: Email
  , password :: f String
  | r
  )

type LoginFields =
  { | CredentialsRep Unlifted () }

type RegisterFields =
  { | CredentialsRep Unlifted (username :: Username) }

--type UpdateFields =
--  { | CredentialsRep + OtherRep Maybe () }

convert :: RequestMethod -> Tuple Method (Maybe Json)
convert = case _ of
  Delete     -> Tuple DELETE Nothing
  Get        -> Tuple GET    Nothing
  Post mJson -> Tuple POST   mJson
  Put mJson  -> Tuple PUT    mJson

formJsonRequest
  :: BaseURL
  -> Maybe Token
  -> RequestOptions
  -> Request Json
formJsonRequest (BaseURL baseUrl) mToken { endpoint, method } =
  let
    Tuple httpMethod mJson = convert method
  in
    { content: map RequestBody.json mJson
    , headers: case mToken of
        Nothing -> []
        Just (Token str) -> [RequestHeader "Authorization" $ "Token " <> str]
    , method: Left httpMethod
    , password: Nothing
    , responseFormat: ResponseFormat.json
    , url: baseUrl <> print endpointCodec endpoint
    , username: Nothing
    , withCredentials: false
    }

login
  :: forall m
   . MonadAff m
  => BaseURL
  -> LoginFields
  -> m (Either String (Tuple Token Profile))
login baseUrl fields =
  let method = Post $ Just $ encodeJson { user: fields }
  in  requestUser baseUrl { endpoint: Login, method }

register
  :: forall m
   . MonadAff m
  => BaseURL
  -> RegisterFields
  -> m (Either String (Tuple Token Profile))
register baseUrl fields =
  let method = Post $ Just $ encodeJson { user: fields }
  in  requestUser baseUrl { endpoint: Users, method }

requestUser
  :: forall m
   . MonadAff m
  => BaseURL
  -> RequestOptions
  -> m (Either String (Tuple Token Profile))
requestUser baseUrl options = do
  response <- liftAff $ request $ formJsonRequest baseUrl Nothing options
  pure $ lmap printResponseFormatError response.body
    >>= decodeJson
    >>= (_ .: "user")
    >>= decodeProfileAndToken

decodeProfileAndToken :: Json -> Either String (Tuple Token Profile)
decodeProfileAndToken json = do
  str <- (decodeJson <=< (_ .: "token") <=< decodeJson) json
  profile <- decodeJson json
  pure $ Tuple (Token str) profile

tokenKey :: String
tokenKey = "token"

readToken   :: Effect (Maybe Token)
readToken = do
  mStr <- window >>= localStorage >>= getItem tokenKey 
  pure $ map Token mStr

removeToken :: Effect Unit
removeToken =
  window >>= localStorage >>= removeItem tokenKey


writeToken  :: Token -> Effect Unit
writeToken (Token str) =
  window >>= localStorage >>= setItem tokenKey str
