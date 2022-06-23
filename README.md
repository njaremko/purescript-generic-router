# Generic Router

This package provides a generic router, you just need to provide a function 
that can parse a path from an arbitrary request, and optionally parse arbitrary context out of a request

# How to use
Here's an example using the `purescript-deno` package.

Requires you to have [deno](https://deno.land) installed

Put this in your `Main.purs` file:

```purescript
module Main
  ( main
  ) where

import Prelude
import Data.Argonaut (decodeJson, encodeJson, stringify)
import Data.Either (fromRight)
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
import Foreign.Object as Object
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

    encodedAsTupleArray = encodeJson $ params

    (casted :: Array (Tuple String String)) = fromRight [] $ decodeJson encodedAsTupleArray
  pure $ createResponse (stringify $ encodeJson $ Object.fromFoldable $ casted) response_options

```

Run the following:
```sh
spago build
deno eval 'import { main } from "./output/Main/index.js"; main();'
```

Then open another terminal and run
```sh
curl -H "content-type: application/json" --data '{"x": 1}' localhost:3001/v1/projects/asdf/environments/fdsa/flags/asdf
```
Which will return
```json
{"environment":"fdsa","flag":"asdf","project":"asdf"}
```