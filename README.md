# Generic Router

This package provides a generic router, you just need to provide a function 
that can parse a path from an arbitrary request, and optionally parse arbitrary context out of a request

# How to use
The following is a simple example, but this router can support arbitrary requests, responses, and request contexts. 

You can find more examples in `/examples` or `/test`

```purescript
module Main
  ( main
  ) where

import Prelude
import Router (makeRoute, makeRouter, route)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Router.Method as Method
import Test.Assert (assert)

testRouteMatch :: Effect Unit
testRouteMatch = do
  let
    route1 = makeRoute { path: "/apples", methods: [ Method.GET ] }

    routes = (Map.fromFoldable [ Tuple route1 (\_req _ctx -> "Hello!") ])

    fallbackResponse = "Not Found"

    requestToPath = (\_req -> "/apples")

    requestToMethod = (\_req -> Method.GET)

    requestToContext = (\_req -> {})

    router =
      makeRouter
        { routes
        , fallbackResponse
        , requestToPath
        , requestToMethod
        , requestToContext
        }

    resp = route router ""
  assert $ resp == "Hello!"
```
