module Router
  ( Context
  , Router
  , Route
  , makeRouter
  , makeRoute
  , route
  ) where

import Prelude
import Data.Array (all, any, zip, (!!))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits (charAt)
import Data.Tuple (Tuple(..))
import Record.Unsafe.Union (unsafeUnion)

newtype Route
  = Route
  { methods :: Array String
  , path :: String
  , paramIndexes :: Map String Int
  }

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

emptyRoute :: Route
emptyRoute = Route { methods: [], path: "", paramIndexes: Map.empty }

newtype Router context request response
  = Router
  { routeMap :: Map Route (request -> Context context -> response)
  , fallback :: response
  , requestToPath :: request -> String
  , requestToMethod :: request -> String
  , requestToContext :: request -> Record context
  }

type Context r
  = { path :: String
    , params :: Map String String
    | r
    }

makeRoute :: String -> Array String -> Route
makeRoute path methods =
  let
    pattern = Pattern "/"

    params = String.split pattern path

    paramIndexes = foldrWithIndex (\i new acc -> if charAt 0 new == Just ':' then Map.insert (String.drop 1 new) i acc else acc) Map.empty params
  in
    Route { path, methods, paramIndexes }

type RequestToPath request
  = request -> String

type RequestToMethod request
  = request -> String

type RequestToContext request context
  = request -> Record context

makeRouter ::
  forall context request response.
  Map Route (request -> Context context -> response) ->
  response ->
  RequestToPath request ->
  RequestToMethod request ->
  RequestToContext request context ->
  Router context request response
makeRouter routeMap fallback requestToPath requestToMethod requestToContext = Router { routeMap, fallback, requestToPath, requestToMethod, requestToContext }

route :: forall context request response. Router context request response -> request -> response
route (Router { routeMap, requestToPath, requestToContext, requestToMethod, fallback }) request =
  let
    path = requestToPath request

    partialContext = requestToContext request

    fallbackRoute = Tuple emptyRoute (\_req _ctx -> fallback)

    Tuple matched handler =
      fromMaybe fallbackRoute
        $ List.head
        $ Map.toUnfoldable
        $ Map.filterKeys routeMatch routeMap

    params = buildParams matched

    context = unsafeUnion partialContext { path, params }
  in
    handler request context
  where
  buildParams :: Route -> Map String String
  buildParams (Route { paramIndexes }) =
    let
      splitPath = String.split (Pattern "/") (requestToPath request)
    in
      Map.mapMaybe (\v -> splitPath !! v) paramIndexes

  routeMatch :: Route -> Boolean
  routeMatch (Route routeToMatch) =
    let
      splitter = Pattern "/"

      splitPattern = String.split splitter (String.toLower routeToMatch.path)

      splitRequestUrl = String.split splitter (String.toLower (requestToPath request))

      zipped = zip splitPattern splitRequestUrl

      methodMatch = any (\method -> method == requestToMethod request) routeToMatch.methods
    in
      methodMatch && all (\(Tuple p r) -> if charAt 0 p == Just ':' then true else p == r) zipped
