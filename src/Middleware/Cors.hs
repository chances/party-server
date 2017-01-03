module Middleware.Cors
    ( getCorsPolicy
    ) where

import           Network.Wai                 (Request)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), Origin,
                                              simpleHeaders)

import           Utils                       (strToBS)

policyFromOrigins :: Maybe ([Origin], Bool) -> CorsResourcePolicy
policyFromOrigins origins = CorsResourcePolicy
    { corsOrigins = origins
    , corsMethods = map strToBS ["GET", "PUT", "POST", "DELETE"]
    , corsRequestHeaders = simpleHeaders
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    -- , corsRequireOrigin = case origins of
    --     Nothing -> False
    --     _       -> True
    -- TODO: Figure out why this doesn't work when deployed
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

generateCorsPolicy :: Maybe String -> CorsResourcePolicy
generateCorsPolicy maybeCorsOrigin = case maybeCorsOrigin of
    Nothing -> policyFromOrigins Nothing
    Just corsOrigin -> policyFromOrigins (Just (([strToBS corsOrigin], False)))

getCorsPolicy :: Maybe String -> (Request -> Maybe CorsResourcePolicy)
getCorsPolicy corsOrigin = policy where
    policy :: Request -> Maybe CorsResourcePolicy
    policy _ = Just (generateCorsPolicy corsOrigin)

-- TODO: Add encompasing middleware that checks the Origin req header
-- See: https://hackage.haskell.org/package/wai-cors-0.2.5/docs/Network-Wai-Middleware-Cors.html#v:cors
-- Check that the value of the Origin header matches it's expectations.
