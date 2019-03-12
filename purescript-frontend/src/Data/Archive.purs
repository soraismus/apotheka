module Apotheka.Data.Archive
  ( Archive(..)
  , decodeArchive
  ) where

import Prelude

import Apotheka.Data.Author (Author)
import Apotheka.Data.Id (Id)
import Apotheka.Data.Link (Link)
import Apotheka.Data.Paper (Paper, decodePaper)
import Apotheka.Data.Title (Title)
import Apotheka.Data.Utils (tryGet)
import Apotheka.Data.Year (Year)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Either (Either)
import Data.Map (Map)
import Data.Set (Set)
import Foreign.Object as FO

type Archive =
  { titles :: Map Id Title
  , authors :: Map Id Author
  , links :: Map Id Link
  , authorsIndex :: Map Id (Set Id)
  , minYear :: Year
  , maxYear :: Year
  , papers :: Array Paper
  }

authorsKey :: String
authorsKey = "b"

authorsIndexKey :: String
authorsIndexKey = "e"

linksKey :: String
linksKey = "c"

papersKey :: String
papersKey = "d"

titlesKey :: String
titlesKey = "a"

maxYearKey :: String
maxYearKey = "g"

minYearKey :: String
minYearKey = "f"

decodeArchive :: Json -> Either String Archive
decodeArchive json = do
  obj          <- decodeJson json
  titles       <- obj .: titlesKey
  authors      <- obj .: authorsKey
  links        <- obj .: linksKey
  authorsIndex <- obj .: authorsIndexKey
  minYear      <- obj .: minYearKey
  maxYear      <- obj .: maxYearKey
  papers       <- tryGetPapers papersKey titles authors links obj
  pure
    { titles
    , authors
    , links
    , authorsIndex
    , minYear
    , maxYear
    , papers
    }

tryGetPapers
  :: String
  -> Map Id Title
  -> Map Id Author
  -> Map Id Link
  -> FO.Object Json
  -> Either String (Array Paper)
tryGetPapers key titles authors links =
  tryGet key (decodePaper titles authors links)
