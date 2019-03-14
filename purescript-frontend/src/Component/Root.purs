module Apotheka.Component.Root
  ( Query
  , component
  ) where

import Prelude

import Apotheka.Capability.LogMessages (class LogMessages, logDebug)
import Apotheka.Capability.Now (class Now)
import Apotheka.Capability.RequestArchive (class RequestArchive, requestArchive)
import Apotheka.Component.HTML.Utils (_class)
import Apotheka.Component.Utils (afterDuration, getDailyIndex, split)
import Apotheka.Component.HTML.Header (viewHeader)
import Apotheka.Component.HTML.Filtering as Filtering
import Apotheka.Component.HTML.PaperList (viewPaperList)
import Apotheka.Component.HTML.PaperOfTheDay (viewPaperOfTheDay)
import Apotheka.Data.Archive (Archive)
import Apotheka.Data.Author (Author)
import Apotheka.Data.Id (Id)
import Apotheka.Data.Paper (Paper)
import Apotheka.Data.Present (present)
import Apotheka.Data.Year (Year, toInt)
import Apotheka.Data.WrappedDate (WrappedDate(WrappedDate))
import Apotheka.Foreign.Slider (SliderYears, onSliderUpdate)
import Data.Array ((!!))
import Data.Array as Array
import Data.Date (Date)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (all, foldM, foldr, for_)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(Nil), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (ignoreCase)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource (eventSource', eventSource_')

data RenderAmount = RenderAll | RenderSome

derive instance genericRenderAmount :: Generic RenderAmount _

instance showRenderAmount :: Show RenderAmount where
  show = genericShow

type StateRec =
  { authors :: Map Id Author
  , authorsIndex :: Map Id (Set Id)
  , dailyIndex :: Int
  , ids :: Set Id
  , filters :: { author :: String, title :: String }
  , years :: { max :: Year, min :: Year }
  , now :: Date
  , papers :: Array Paper
  , overallMaxYear :: Year
  , overallMinYear :: Year
  , renderAmount :: RenderAmount
  , selectedPapers :: Array Paper 
  }

data State
  = NotAsked
  | Loading
  | Loaded StateRec
  | LoadError

derive instance genericState :: Generic State _

instance showState :: Show State where
  show = genericShow

data Query a
  = RequestArchive a
  | RenderMore (H.SubscribeStatus -> a)
  | AddFacet Author a
  | FilterByYear SliderYears (H.SubscribeStatus -> a)
  | HandleFilteringUpdate Filtering.Message a

data FilteringSlot = FilteringSlot

derive instance eqFilteringSlot  :: Eq  FilteringSlot
derive instance ordFilteringSlot :: Ord FilteringSlot

component
  :: forall m
   . MonadAff m
  => LogMessages m
  => Now m
  => RequestArchive m
  => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = NotAsked

  render :: State -> H.ParentHTML Query Filtering.Query FilteringSlot m
  render NotAsked = HH.div_
    [ HH.text "NotAsked"
    , HH.button
        [ HE.onClick $ HE.input_ RequestArchive ]
        [ HH.text "Request archive." ]
    ]
  render Loading = HH.text "Loading"
  render (Loaded stateRec) = view stateRec
  render LoadError = HH.text "LoadError"

  eval :: Query ~> H.ParentDSL State Query Filtering.Query FilteringSlot Void m

  eval (RequestArchive next) = do
    H.put Loading
    requestArchive >>= case _ of
      Just response -> evalResponse response next
      Nothing       -> H.put LoadError *> pure next

  eval (RenderMore reply) = do
    logDebug "RenderMore"
    mFilterState <- H.query FilteringSlot $ H.request Filtering.RequestState
    for_ mFilterState (\filterState ->
      H.modify_ $ mapState (\stateRec@{ papers, years } ->
        stateRec
          { renderAmount = RenderAll
          , selectedPapers =
              filter filterState { papers, renderAmount: RenderAll, years }
          }))
    pure $ reply H.Done

  eval (AddFacet author next) = do
    logDebug $ "AddFacet: " <> show author
    state <- H.get
    forLoaded state (\stateRec -> do
      let facet = { name: present author
                  , titleIds: buildAuthorFilterIds author stateRec
                  }
      void $ H.query FilteringSlot $ H.action $ Filtering.AddFacet facet)
    pure next

  eval (FilterByYear sliderYears@(Tuple minYear maxYear) reply) = do
    logDebug $ "FilterByYear -- " <> show sliderYears
    mFilterState <- H.query FilteringSlot $ H.request Filtering.RequestState
    for_ mFilterState (\filterState -> do
      let years = { max: maxYear, min: minYear }
      H.modify_ $ mapState (\stateRec@{ papers, renderAmount } ->
        stateRec
         { years = years
         , selectedPapers = filter filterState { papers, renderAmount, years }
         }))
    pure $ reply H.Listening

  eval (HandleFilteringUpdate (Filtering.Update filterState) next) = do
    logDebug "HandleFilteringUpdate"
    H.modify_ $ mapState (\stateRec ->
      stateRec
        { filters { author = filterState.author, title = filterState.title }
        , selectedPapers = filter filterState stateRec
        })
    pure next

  evalResponse
    :: forall a
    . { archive :: Archive, date :: WrappedDate }
    -> a
    -> H.ParentDSL State Query Filtering.Query FilteringSlot Void m a
  evalResponse response next = do
    let paperCount = Array.length response.archive.papers
    dailyIndex <- getDailyIndex paperCount
    let stateRec = convert dailyIndex response
    H.put $ Loaded stateRec
    let min = toInt stateRec.overallMinYear
    let max = (toInt stateRec.overallMaxYear) + 1
    let slider = { id: "year-slider"
                 , start: [min, max]
                 , margin: Just 1
                 , limit: Nothing
                 , connect: Just true
                 , direction: Nothing
                 , orientation: Nothing
                 , behavior: Nothing
                 , step: Just 1
                 , range: Just { min, max }
                 }
    logDebug ("RequestArchive -- " <> show slider)
    H.subscribe $ eventSource'
      (onSliderUpdate slider)
      (Just <<< H.request <<< FilterByYear)
    H.subscribe $ eventSource_'
      (afterDuration 750)
      (H.request RenderMore)
    pure next

buildAuthorFilterIds
  :: forall r
   . Author
  -> { authors      :: Map Id Author
     , authorsIndex :: Map Id (Set Id)
     | r
     }
  -> Set Id
buildAuthorFilterIds author { authors, authorsIndex }
  | String.null $ String.trim $ present author = Set.empty
  | otherwise =
      either (const Set.empty) identity do
        regexes <- getRegexes $ present author
        let _reduce = reduce regexes
        pure $ foldrWithIndex _reduce Set.empty authors
  where
    _getRegexes :: Array String -> Either String (List Regex)
    _getRegexes =
      foldM
        (\regexes str -> case Regex.regex str ignoreCase of
            Left error -> Left $ "Error on '" <> str <> "': " <> error
            Right regex -> Right $ (regex : regexes))
        Nil

    getRegexes :: String -> Either String (List Regex)
    getRegexes = _getRegexes <<< split

    -- NB: I think this is intended for a binominal/trinominal author.
    match :: List Regex -> String -> Boolean
    match regexes' fullName' =
      all (\regex -> Regex.test regex fullName') regexes'

    reduce :: List Regex -> Id -> Author -> Set Id -> Set Id
    reduce regexes' id' author' ids' =
      if match regexes' $ present author'
        then case Map.lookup id' authorsIndex of
          Nothing -> ids'
          Just _ids -> Set.union ids' _ids
        else ids'

convert :: Int -> { archive :: Archive, date :: WrappedDate } -> StateRec
convert index { archive, date: WrappedDate _date } =
  let
    ids = getAllIds archive.papers
  in
    { authors: archive.authors
    , authorsIndex: archive.authorsIndex
    , dailyIndex: index
    , filters: { author: "", title: "" }
    , ids
    , years: { max: archive.maxYear, min: archive.minYear }
    , now: _date
    , overallMaxYear: archive.maxYear
    , overallMinYear: archive.minYear
    , papers: archive.papers
    , selectedPapers: archive.papers
    , renderAmount: RenderSome
    }

filter
  :: forall r0 r1 r2 r3 r4
   . { facets :: Array { titleIds :: Set Id | r0 }
     , idsForAuthor :: Set Id
     , idsForTitle :: Set Id
     , includeUnknown :: Boolean
     , isIrrelevant :: { author :: Boolean
                       , facet :: Boolean
                       , title :: Boolean
                       | r1
                       }
     | r2
     }
  -> { papers :: Array Paper 
     , renderAmount :: RenderAmount
     , years :: { max :: Year, min :: Year | r3 }
     | r4
    }
  -> Array Paper
filter filterState { renderAmount, papers, years } = case renderAmount of
    RenderSome -> _filter $ Array.take 25 papers
    RenderAll  -> _filter papers
  where
    _filter = Array.filter $ isSelected filterState years

forLoaded :: forall m. Monad m => State -> (StateRec -> m Unit) -> m Unit
forLoaded (Loaded stateRec) f = f stateRec
forLoaded _                 _ = pure unit

getAllIds :: Array Paper -> Set Id
getAllIds = foldr (Set.insert <<< _.titleId) Set.empty

isSelected
  :: forall r0 r1 r2 r3
   . { facets :: Array { titleIds :: Set Id | r0 }
     , idsForAuthor :: Set Id
     , idsForTitle :: Set Id
     , includeUnknown :: Boolean
     , isIrrelevant :: { author :: Boolean
                       , facet :: Boolean
                       , title :: Boolean
                       | r1
                       }
     | r2
     }
  -> { max :: Year, min :: Year | r3 }
  -> Paper
  -> Boolean
isSelected filters@{ facets, includeUnknown } { max, min } paper =
  let
    id = paper.titleId

    isIrrelevant = filters.isIrrelevant

    isYearSelected = maybe includeUnknown
      (\year -> year >= min && year <= max)
      paper.yearMaybe
  in
    isYearSelected
      && (isIrrelevant.title || Set.member id filters.idsForTitle)
      && (isIrrelevant.author || Set.member id filters.idsForAuthor)
      && (isIrrelevant.facet ||  all (Set.member id <<< _.titleIds) facets)

mapState :: (StateRec -> StateRec) -> State -> State
mapState f (Loaded stateRec) = Loaded $ f stateRec
mapState f state             = state

view
  :: forall m
   . MonadAff m
  => LogMessages m
  => Now m
  => RequestArchive m
  => StateRec
  -> H.ParentHTML Query Filtering.Query FilteringSlot m
view stateRec@{ dailyIndex, ids, papers, selectedPapers } =
  HH.div
    [ _class "container" ]
    [ viewHeader $ Array.length selectedPapers
    , viewPaperOfTheDay AddFacet (papers !! dailyIndex)
    , HH.slot
        FilteringSlot
        Filtering.component
        { ids, papers }
        (HE.input HandleFilteringUpdate)
    , viewPaperList AddFacet stateRec
    ]
