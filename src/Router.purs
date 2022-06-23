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
  { routeMap :: Map String (request -> context -> response)
  , fallback :: response
  , requestToPath :: request -> String
  , requestToContext :: request -> context
  }

type Router
  = GenericRouter Context

type Context
  = { path :: String
    , params :: Map String String
    }

makeRouter ::
  forall context request response.
  Map String (request -> context -> response) ->
  response ->
  (request -> String) ->
  (request -> context) ->
  GenericRouter context request response
makeRouter routeMap fallback requestToPath requestToContext = Router { routeMap, fallback, requestToPath, requestToContext }

route :: forall context request response. GenericRouter context request response -> request -> response
route router@(Router { requestToPath, requestToContext }) request =
  let
    path = requestToPath request

    context = requestToContext request
  in
    (findMatch router path) request context
  where
  findMatch :: GenericRouter context request response -> String -> (request -> context -> response)
  findMatch (Router { routeMap, fallback }) path =
    let
      handler = fromMaybe (\_req -> \_ctx -> fallback) $ List.head $ Map.values $ Map.filterKeys (\k -> routeMatch k path) routeMap
    in
      handler

  routeMatch :: String -> String -> Boolean
  routeMatch pattern requestUrl =
    let
      splitPattern = String.split (Pattern "/") pattern

      splitRequestUrl = String.split (Pattern "/") requestUrl

      zipped = zip splitPattern splitRequestUrl
    in
      all (\(Tuple p r) -> if charAt 0 p == Just ':' then true else p == r) zipped
