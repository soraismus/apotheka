module Apotheka.Component.Utils
  ( afterDuration
  , deleteWhen
  , getDailyIndex
  , inArray
  , split
  ) where

import Prelude

import Apotheka.Capability.Now (class Now, nowDate)
import Data.Array (deleteAt, elemIndex, findIndex)
import Data.Date (Date, canonicalDate, diff)
import Data.Date.Component (Month(..))
import Data.Enum (toEnum)
import Data.Int (fromNumber)
import Data.Maybe (Maybe, fromJust, fromMaybe, isJust, maybe)
import Data.String as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Time.Duration (Days(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

foreign import afterDuration :: Int -> Effect Unit -> Effect (Effect Unit)

deleteWhen :: forall a. (a -> Boolean) -> Array a -> Array a
deleteWhen _ [] = []
deleteWhen f xs =
  maybe
    xs
    (\i -> unsafePartial $ fromJust (deleteAt i xs))
    (findIndex f xs)

getDateDiffMaybe :: Date -> Maybe Days
getDateDiffMaybe date = do
  baseDate <- maybeBaseDate
  pure $ diff date baseDate

getDailyIndex :: forall m. Now m => Int -> m Int
getDailyIndex max = nowDate >>= (pure <<< fromMaybe 0 <<< getIndexMaybe max)

getIndexMaybe :: Int -> Date -> Maybe Int
getIndexMaybe max date = do
  Days number <- getDateDiffMaybe date
  int <- fromNumber number
  pure $ int `mod` max

inArray :: forall a. Eq a => a -> Array a -> Boolean
inArray x xs = isJust $ elemIndex x xs

maybeBaseDate :: Maybe Date
maybeBaseDate = do
  year <- toEnum 2019
  day <- toEnum 1
  pure $ canonicalDate year January day

split :: String -> Array String
split = _split <<< String.trim

_split :: String -> Array String
_split str
  | String.null str = []
  | otherwise       = String.split (Pattern " ") str
