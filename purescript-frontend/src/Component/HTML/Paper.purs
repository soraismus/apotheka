module Apotheka.Component.HTML.Paper
  ( viewPaper
  ) where

import Prelude

import Apotheka.Component.HTML.Utils (_class)
import Apotheka.Component.Utils (split)
import Apotheka.Component.ViewUtils (padLeft3)
import Apotheka.Data.Author (Author)
import Apotheka.Data.Link (Link)
import Apotheka.Data.Paper (Paper)
import Apotheka.Data.Present (present)
import Apotheka.Data.Title (Title)
import Apotheka.Data.Year (Year)
import Data.Array as Array
import Data.Either (Either(Left, Right), either, hush)
import Data.Filterable (filterMap)
import Data.Foldable (foldr)
import Data.List (List(Cons, Nil), (:))
import Data.List as List
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (ignoreCase)
import Data.Tuple (Tuple(Tuple))
import Halogen (HTML, IProp)
import Halogen.HTML (a, p, span, span_, text)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (href)

type Interval a = Tuple a a
type Match = String
type NonMatch = String

explode
  :: List (Tuple Int Int)
  -> String
  -> List (Either NonMatch Match)
explode intervals str = go 0 intervals
  where
  go :: Int -> List (Tuple Int Int) -> List (Either NonMatch Match)
  go n = case _ of
    Nil ->
      let
        length = String.length str
      in
        if n == length
          then Nil
          else (Left $ _slice n length str) : Nil

    (Cons (Tuple i j) tail) ->
      if n < i
        then (Left  $ _slice n i str)
               : (Right $ _slice i j str)
               : (go j tail)
        else (Right $ _slice i j str)
             : (go j tail)

highlightPatches :: forall i p. String -> Array String -> Array (HTML i p)
highlightPatches haystack needles =
  toArray $ map viewSegment segments
  where
    getSegments :: List String -> String -> List (Either NonMatch Match)
    getSegments =
      explode
        <<< (foldr insertInterval Nil)
        <<< (filterMap ((_ $ haystack) <<< interval))

    segments :: List (Either NonMatch Match)
    segments = getSegments (toList needles) haystack

insertInterval
  :: forall a
   . Ord a
  => Interval a
  -> List (Interval a)
  -> List (Interval a)
insertInterval x_to_y@(Tuple x y) = case _ of
  Nil -> (Tuple x y) : Nil
  Cons (Tuple x' y') zs
    | y < x'    -> (Tuple x y) : (Tuple x' y') : zs
    | y' < x    -> (Tuple x' y') : insertInterval x_to_y zs
    | otherwise -> (Tuple (min x x') (max y y')) : zs

interval :: String -> String -> Maybe (Tuple Int Int)
interval str0 str1
  | String.null str0 = Nothing
  | otherwise = do
      regex <- hush $ Regex.regex str0 ignoreCase
      index <- Regex.search regex str1
      pure $ Tuple index (index + String.length str0)

_slice :: Int -> Int -> String -> String
_slice i j str = fromMaybe "" $ SCU.slice i j str

toArray :: forall a. List a -> Array a
toArray list = Array.fromFoldable list

toList :: forall a. Array a -> List a
toList array = List.fromFoldable array

viewAuthor
  :: forall i p
   . (forall a. Author -> a -> p a)
  -> Maybe String
  -> Author
  -> HTML i p
viewAuthor mkQuery maybeFilter author =
  let
    str = present author
  in
    span
      [ _class "author"
      , onClick $ input_ $ mkQuery author
      ]
      (maybe
        [text str]
        (highlightPatches str <<< split)
        maybeFilter)

viewAuthors
  :: forall i p
   . (forall a. Author -> a -> p a)
  -> Array Author
  -> Maybe String
  -> HTML i p
viewAuthors mkQuery authors maybeFilter =
  span_ $ map (viewAuthor mkQuery maybeFilter) authors

viewCitations :: forall i p. Array Title -> HTML i p
viewCitations citations =
  text _text
  where
    count = Array.length citations
    _text = if count == 0 then "" else " (cited by " <> show count <> ")"

viewEditLink
  :: forall i p r
   . { file :: Int, line :: Int | r }
  -> HTML i p
viewEditLink { file, line } =
  a
    [ _class "subtle-link edit"
    , href editLink
    ]
    [ text "(edit)" ]
  where
    editLink =
      "https://github.com"
        <> "/mitchellwrosen/haskell-papers"
        <> "/edit/master/papers"
        <> (padLeft3 $ show file)
        <> ".yaml#L"
        <> show line

viewPaper
  :: forall r p i
   . (forall a. Author -> a -> i a)
  -> Maybe String
  -> Maybe String
  -> Paper
  -> Tuple (Array (IProp (class :: String | r) i)) (Array (HTML p i))
viewPaper mkQuery mAuthorFilter mTitleFilter paper =
  Tuple
    [ _class "paper" ]
    [ viewTitle paper.title (Array.head paper.links) mTitleFilter
    , p
      [ _class "details" ]
      [ viewAuthors mkQuery paper.authors mAuthorFilter
      , viewYearMaybe paper.yearMaybe
      , viewCitations paper.citations
      ]
    , viewEditLink paper.loc
    ]

viewSegment :: forall i p. Either NonMatch Match -> HTML i p
viewSegment =
  either
    text
    (span [ _class "highlight" ] <<< Array.singleton <<< text)

viewTitle :: forall i p. Title -> Maybe Link -> Maybe String -> HTML i p
viewTitle title maybeLink maybeFilter =
  p
    [ _class "title" ]
    linkNodes
  where
    titleNodes = maybe
      [text $ present title]
      (highlightPatches (present title) <<< split)
      maybeFilter
    linkNodes = maybe
      titleNodes
      (\link -> [a [_class "link", href $ present link] titleNodes])
      maybeLink

viewYearMaybe :: forall i p. Maybe Year -> HTML i p
viewYearMaybe maybeYear =
  maybe
    (text "")
    (\year -> text $ " [" <> present year <> "] ")
    maybeYear
