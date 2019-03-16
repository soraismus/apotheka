module Apotheka.Form.Validation
  ( FormError
  , emailFormat
  , errorToString
  , usernameFormat
  , maxLength
  , minLength
  , required
  ) where

import Prelude

import Apotheka.Data.Avatar (Avatar)
import Apotheka.Data.Avatar as Avatar
import Apotheka.Data.Email (Email(..))
import Apotheka.Data.Username (Username)
import Apotheka.Data.Username as Username
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String as String
import Formless as F

data FormError
  = InvalidAvatar
  | InvalidEmail
  | InvalidUsername
  | Required
  | TooLong
  | TooShort

errorToString :: FormError -> String
errorToString = case _ of
  InvalidAvatar   -> "Invalid image URL"
  InvalidEmail    -> "Invalid email address"
  InvalidUsername -> "Invalid username"
  Required        -> "This field is required."
  TooLong         -> "Too many characters entered"
  TooShort        -> "Not enough characters entered"

avatarFormat :: ∀ form m. Monad m => F.Validation form m FormError String Avatar
avatarFormat = F.hoistFnE_ $ note InvalidAvatar <<< Avatar.parse

cond :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
cond f err a = if f a then pure a else Left err

emailFormat :: ∀ form m. Monad m => F.Validation form m FormError String Email
emailFormat = F.hoistFnE_ $ map Email <<< cond (String.contains (String.Pattern "@")) InvalidEmail

maxLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
maxLength n = F.hoistFnE_ $ cond (\str -> String.length str <= n) TooLong

minLength :: ∀ form m. Monad m => Int -> F.Validation form m FormError String String
minLength n = F.hoistFnE_ $ cond (\str -> String.length str > n) TooShort

required :: ∀ form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ cond (_ /= mempty) Required

toOptional :: ∀ form m a b
   . Monoid a
  => Eq a
  => Monad m
  => F.Validation form m FormError a b
  -> F.Validation form m FormError a (Maybe b)
toOptional v = F.Validation \form val ->
  case val == mempty of
    true -> pure (pure Nothing)
    _ -> (map <<< map) Just (F.runValidation v form val)

usernameFormat :: ∀ form m. Monad m => F.Validation form m FormError String Username
usernameFormat = F.hoistFnE_ $ note InvalidUsername <<< Username.parse
