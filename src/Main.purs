module Main
  ( main
  ) where

import Prelude
import Data.Argonaut (encodeJson, stringify)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Tuple (Tuple(..))
import Deno as Deno
import Deno.Dotenv as Dotenv
import Deno.Http (Response, createResponse, hContentTypeHtml, hContentTypeJson, serveListener)
import Deno.Http.Request (Request)
import Deno.Http.Request as Request
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)
import Router (Router, Context, makeRouter)
import Router as Router

type AppRouter
  = Router ()

type AppContext
  = Context ()

routeToContext :: Request -> {}
routeToContext _req = {}

main :: Effect Unit
main = do
  log "Let's get cookin 🍝"
  e <-
    Dotenv.configSync $ Just
      $ { export: Just true
        , allowEmptyValues: Nothing
        , defaults: Nothing
        , example: Nothing
        , path: Nothing
        , safe: Nothing
        }
  let
    baseUrl = fromMaybe "" $ Map.lookup "APP_URL" $ e

    routes =
      Map.fromFoldable
        [ Tuple "/" indexRoute
        , Tuple "/v1/projects/:project/environments/:environment/flags/:flag" jsonEcho
        ]

    router =
      makeRouter
        routes
        (pure $ createResponse "Not Found" (Just { headers: Just $ Map.fromFoldable [ Tuple "content-type" "text/plain" ], status: Just 404, statusText: Just "Not Found" }))
        (\req -> replace (Pattern baseUrl) (Replacement "") $ Request.url req)
        routeToContext

    handler = Router.route router
  listener <- Deno.listen { port: 3001 }
  launchAff_ $ serveListener listener handler Nothing

indexRoute :: Request → AppContext -> Aff Response
indexRoute _req _ctx =
  let
    payload =
      """
    <html>
      <head></head>
      <body>
        <div>
          Hello World!
        </div>
      </body>
    </html>
    """

    headers = Just $ Map.fromFoldable [ hContentTypeHtml ]

    response_options = Just { headers, status: Nothing, statusText: Nothing }
  in
    pure $ createResponse payload response_options

jsonEcho :: Request → AppContext -> Aff Response
jsonEcho req { params } = do
  _payload <- Request.json req
  let
    headers = Just $ Map.fromFoldable [ hContentTypeJson ]

    response_options = Just { headers, status: Nothing, statusText: Nothing }
  pure $ createResponse (stringify $ encodeJson $ params) response_options
