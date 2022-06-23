module Router
  ( Context
  , GenericRouter
  , Router
  , makeRouter
  , route
  ) where

import Prelude
import Data.Array (all, zip)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits (charAt)
import Data.Tuple (Tuple(..))

newtype GenericRouter context request response
  = Router
  { routeMap :: Map String (request -> Context context -> response)
  , fallback :: response
  , requestToContext :: request -> Context context
  }

type Router
  = GenericRouter ( params :: Map String String )

type Context r
  = { path :: String
    | r
    }

makeRouter ::
  forall context request response.
  Map String (request -> Context context -> response) ->
  response ->
  (request -> Context context) ->
  GenericRouter context request response
makeRouter routeMap fallback requestToContext = Router { routeMap, fallback, requestToContext }

route :: forall context request response. GenericRouter context request response -> request -> response
route router@(Router { requestToContext }) request =
  let
    context@{ path } = requestToContext request
  in
    (findMatch router path) request context
  where
  findMatch :: GenericRouter context request response -> String -> (request -> Context context -> response)
  findMatch (Router { routeMap, fallback }) path =
    let
      handler = fromMaybe (\_req -> \_ctx -> fallback) $ List.head $ Map.values $ Map.filterKeys (\k -> routeMatch k path) routeMap
    in
      handler

  routeMatch :: String -> String -> Boolean
  routeMatch pattern requestUrl =
    let
      splitter = Pattern "/"

      splitPattern = String.split splitter pattern

      splitRequestUrl = String.split splitter requestUrl

      zipped = zip splitPattern splitRequestUrl
    in
      all (\(Tuple p r) -> if charAt 0 p == Just ':' then true else p == r) zipped
