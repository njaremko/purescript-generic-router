module Router
  ( Context
  , Router
  , makeRouter
  , route
  ) where

import Prelude
import Data.Array (all, filter, zip)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits (charAt)
import Data.Tuple (Tuple(..))
import Record.Unsafe.Union (unsafeUnion)

newtype Router context request response
  = Router
  { routeMap :: Map String (request -> Context context -> response)
  , fallback :: response
  , requestToPath :: request -> String
  , requestToContext :: request -> Record context
  }

type Context r
  = { path :: String
    , params :: Map String String
    | r
    }

makeRouter ::
  forall context request response.
  Map String (request -> Context context -> response) ->
  response ->
  (request -> String) ->
  (request -> Record context) ->
  Router context request response
makeRouter routeMap fallback requestToPath requestToContext = Router { routeMap, fallback, requestToPath, requestToContext }

route :: forall context request response. Router context request response -> request -> response
route (Router { routeMap, requestToPath, requestToContext, fallback }) request =
  let
    path = requestToPath request

    partialContext = requestToContext request

    Tuple matched handler = fromMaybe ((Tuple "" (\_req _ctx -> fallback))) $ List.head $ Map.toUnfoldable $ Map.filterKeys (\k -> routeMatch k path) routeMap

    -- TODO match route and build params simulatanously
    params = buildParams matched path

    context = unsafeUnion partialContext { path, params }
  in
    handler request context
  where
  buildParams :: String -> String -> Map String String
  buildParams matchedRoute path =
    let
      splitter = Pattern "/"

      splitMatchedRoute = String.split splitter matchedRoute

      splitPath = String.split splitter path
    in
      Map.fromFoldable
        $ map (\(Tuple k v) -> Tuple (String.drop 1 k) v)
        $ filter (\(Tuple p _) -> charAt 0 p == Just ':')
        $ zip splitMatchedRoute splitPath

  routeMatch :: String -> String -> Boolean
  routeMatch pattern requestUrl =
    let
      splitter = Pattern "/"

      splitPattern = String.split splitter pattern

      splitRequestUrl = String.split splitter requestUrl

      zipped = zip splitPattern splitRequestUrl
    in
      all (\(Tuple p r) -> if charAt 0 p == Just ':' then true else p == r) zipped
