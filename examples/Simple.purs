module Simple where

import Prelude
import Router as Router
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Router.Method as Method
import Test.Assert (assert)

testRouteMatch :: Effect Unit
testRouteMatch = do
  let
    route1 = Router.makeRoute { path: "/apples", methods: [ Method.GET ] }

    routes = (Map.fromFoldable [ Tuple route1 (\_req _ctx -> "Hello!") ])

    fallbackResponse = "Not Found"

    requestToPath = (\_req -> "/apples")

    requestToMethod = (\_req -> Method.GET)

    requestToContext = (\_req -> {})

    router =
      Router.makeRouter
        { routes
        , fallbackResponse
        , requestToPath
        , requestToMethod
        , requestToContext
        }

    resp = Router.route router ""
  assert $ resp == "Hello!"
