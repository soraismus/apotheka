module Apotheka.Component.HTML.Filtering
  ( Facet
  , Input
  , Message(Update)
  , Query(AddFacet, RequestState)
  , State
  , component
  ) where

import Prelude

import Apotheka.Capability.LogMessages (class LogMessages, logDebug)
import Apotheka.Capability.Now (class Now)
import Apotheka.Component.HTML.Utils (_class)
import Apotheka.Capability.RequestArchive (class RequestArchive)
import Apotheka.Component.Utils (deleteWhen, inArray, split)
import Apotheka.Data.Author (Author)
import Apotheka.Data.Id (Id)
import Apotheka.Data.Paper (Paper)
import Apotheka.Data.Present (present)
import Data.Array as Array
import Data.Either (Either(Left, Right), either)
import Data.Filterable (maybeBool)
import Data.Foldable (all, any, foldM, foldr)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Nothing))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (ignoreCase)
import DOM.HTML.Indexed.InputType as InputType
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)

data Query a
  = AddFacet Facet a
  | AddFacetFromFilter a
  | FilterByAuthor String a
  | FilterByTitle String a
  | RemoveFacet String a
  | RequestState (State -> a)
  | SetIncludeUnknown Boolean a

data Message = Update State

type Facet =
  { name :: String
  , titleIds :: Set Id
  }

type State =
  { author :: String
  , facets :: Array Facet
  , ids :: Set Id
  , idsForAuthor :: Set Id
  , idsForTitle :: Set Id
  , includeUnknown :: Boolean
  , isIrrelevant :: { author :: Boolean, facet :: Boolean, title :: Boolean }
  , papers :: Array Paper
  , title :: String
  }

isEnter :: KeyboardEvent -> Boolean
isEnter keyboardEvent = (key keyboardEvent) == "Enter"

pushMap
  :: forall f g a b
   . Functor f
  => (a -> f b)
  -> (forall c. c -> g c)
  -> a
  -> f (g Unit)
pushMap f g = f >>> \fb -> map (const $ g unit) fb

infixr 5 pushMap as >.>

viewAuthorSearchBox :: forall i. String -> H.HTML i Query
viewAuthorSearchBox authorName =
  HH.div
    [ _class "author-search" ]
    [ HH.input
      [ _class "author-search-box"
      , HP.placeholder "Search authors"
      , HP.type_ InputType.InputText
      , HP.value authorName
      , HE.onValueInput $ HE.input $ FilterByAuthor
      , HE.onKeyUp $ (maybeBool isEnter >.> AddFacetFromFilter)
      ]
    ]

viewIncludeUnknown :: forall i. H.HTML i Query
viewIncludeUnknown = HH.div
  [ _class "include-unknown" ]
  [ HH.input
    [ _class "include-unknown-checkbox"
    , HP.type_ InputType.InputCheckbox
    , HP.checked true
    , HE.onChecked (HE.input SetIncludeUnknown)
    ]
  , HH.text "Include papers with unknown year of publication "
  ]

viewFacet :: forall i. String -> H.HTML i Query
viewFacet str =
  HH.div
    [ _class "facet"
    , HE.onClick $ HE.input_ $ RemoveFacet str
    ]
    [ HH.text str ]

viewFacets :: forall i. Array String -> H.HTML i Query
viewFacets strs =
  HH.div
    [ _class "facets" ]
    (map viewFacet strs)

viewFiltering
  :: forall i r
   . { title :: String
     , author :: String
     , facets :: Array { name :: String, titleIds :: Set Id }
     | r
     }
  -> H.HTML i Query
viewFiltering { title, author, facets } =
  HH.p_
    [ viewTitleSearchBox title
    , viewAuthorSearchBox author
    , viewFacets $ map _.name facets
    , viewIncludeUnknown
    , HH.div [ HP.id_ "year-slider" ] []
    ]

viewTitleSearchBox :: forall i . String -> H.HTML i Query
viewTitleSearchBox _filter =
  HH.div
    [ _class "title-search" ]
    [ HH.input
      [ _class "title-search-box"
      , HP.placeholder "Search titles"
      , HP.type_ InputType.InputText
      , HP.value _filter
      , HE.onValueInput $ HE.input FilterByTitle
      ]
    ]

-- | -------------------------------------------------------------------------

getInitialState :: Input -> State
getInitialState { ids, papers } =
  { author: ""
  , facets: []
  , includeUnknown: true
  , ids
  , idsForAuthor: ids
  , idsForTitle: ids
  , isIrrelevant: { author: true, facet: true, title: true }
  , papers
  , title: ""
  }

type Input =
  { ids :: Set Id
  , papers :: Array Paper
  }

component
  :: forall m
   . MonadAff m
  => LogMessages m
  => Now m
  => RequestArchive m
  => H.Component HH.HTML Query Input Message m
component = H.lifecycleComponent
  { initialState: getInitialState
  , eval
  , render
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
  render :: State -> H.ComponentHTML Query
  render = viewFiltering

  eval :: Query ~> H.ComponentDSL State Query Message m

  eval (AddFacet facet next) = do
    logDebug $ "Filtering.AddFacet: " <> show facet
    facets <- H.gets _.facets
    unless (inArray facet.name $ map _.name facets) do
       newState <- H.modify (\filterState ->
         filterState
           { facets = Array.snoc facets facet
           , isIrrelevant { facet = false }
           })
       H.raise (Update newState)
    pure next

  eval (AddFacetFromFilter next) = do
    logDebug "AddFacetFromFilter"
    author <- H.gets _.author
    unless (String.null author) do
       newState <- H.modify (\state ->
         state { author = ""
               , facets = getFacets author state
               , idsForAuthor = (Set.empty :: Set Id)
               , isIrrelevant { author = true, facet = false }
               })
       H.raise (Update newState)
    pure next

  eval (FilterByAuthor author next) = do
    logDebug $ "FilterByAuthor: " <> author
    newState <- H.modify (\state ->
      state { author = author
            , idsForAuthor = getIdsForAuthor author state
            , isIrrelevant { author = String.null $ String.trim author }
            })
    H.raise (Update newState)
    pure next

  eval (FilterByTitle title next) = do
    logDebug $ "FilterByTitle: " <> title
    newState <- H.modify (\state ->
      state { title = title
            , idsForTitle = getIdsForTitle title state
            , isIrrelevant { title = String.null $ String.trim title }
            })
    H.raise (Update newState)
    pure next

  eval (RemoveFacet facetName next) = do
    logDebug $ "RemoveFacet: " <> facetName
    facets <- H.gets _.facets
    let facetNames = map _.name facets
    when (inArray facetName facetNames) do
       newState <- H.modify (\state ->
         state { facets = deleteWhen ((_ == facetName) <<< _.name) facets
               , isIrrelevant { facet = Array.length facets == 1 }
               })
       H.raise (Update newState)
    pure next

  eval (RequestState reply) = H.get >>= (pure <<< reply)

  eval (SetIncludeUnknown includeUnknown next) = do
    logDebug $ "SetIncludeUnknown " <> show includeUnknown
    newState <- H.modify (_ { includeUnknown = includeUnknown })
    H.raise (Update newState)
    pure next

getFacets
  :: forall r
   . String
  -> { idsForAuthor :: Set Id, facets :: Array Facet | r }
  -> Array Facet
getFacets name { idsForAuthor, facets } =
  if inArray name $ map _.name facets
    then facets
    else Array.snoc facets { name: name, titleIds: idsForAuthor }

getIdsForAuthor
  :: forall r
   . String
  -> { ids :: Set Id, papers :: Array Paper | r }
  -> Set Id
getIdsForAuthor str { ids, papers }
  | String.null $ String.trim str = ids
  | otherwise =
      either (const Set.empty) identity do
        regexes <- getRegexes str
        let _reduce = reduce regexes
        pure $ foldr _reduce Set.empty papers
  where
    _getRegexes :: Array String -> Either String (List Regex)
    _getRegexes =
      foldM
        (\_regexes _str -> case Regex.regex _str ignoreCase of
            Left _error -> Left $ "Error on '" <> _str <> "': " <> _error
            Right _regex -> Right $ (_regex : _regexes))
        Nil

    getRegexes :: String -> Either String (List Regex)
    getRegexes = _getRegexes <<< split

    match :: List Regex -> Array Author -> Boolean
    match regexes' authors' =
      all
        (\_regex ->
          any
            (\_author -> Regex.test _regex $ present _author)
            authors')
        regexes'

    reduce :: List Regex -> Paper -> Set Id -> Set Id
    reduce regexes' paper' ids' =
      if match regexes' paper'.authors
        then Set.insert paper'.titleId ids'
        else ids'

getIdsForTitle
  :: forall r
   . String
  -> { ids :: Set Id, papers :: Array Paper | r }
  -> Set Id
getIdsForTitle str { ids, papers }
  | String.null $ String.trim str = ids
  | otherwise =
      either (const Set.empty) identity do
        regexes <- getRegexes str
        let _reduce = reduce regexes
        pure $ foldr _reduce Set.empty papers
  where
    _getRegexes :: Array String -> Either String (List Regex)
    _getRegexes =
      foldM
        (\_regexes _str -> case Regex.regex _str ignoreCase of
            Left _error -> Left $ "Error on '" <> _str <> "': " <> _error
            Right _regex -> Right $ (_regex : _regexes))
        Nil

    getRegexes :: String -> Either String (List Regex)
    getRegexes = _getRegexes <<< split

    reduce :: List Regex -> Paper -> Set Id -> Set Id
    reduce regexes' paper' ids' =
      let _str = present paper'.title
      in if all (\_regex -> Regex.test _regex _str) regexes'
           then Set.insert paper'.titleId ids'
           else ids'
