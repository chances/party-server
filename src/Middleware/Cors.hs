module Middleware.Cors
    ( getCorsPolicy
    ) where

import           Data.List.Split             (splitOn)
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
    , corsRequireOrigin = case origins of
        Nothing -> False
        _       -> False -- TODO: Should be True
        -- TODO: Figure out why accessing app directly doesn't send Origin header
    , corsIgnoreFailures = False
    }

generateCorsPolicy :: Maybe String -> CorsResourcePolicy
generateCorsPolicy maybeCorsOrigins = case maybeCorsOrigins of
    Nothing -> policyFromOrigins Nothing
    -- ([Origin], credentialsUsedToAccessTheResource :: Bool)
    Just origins -> let originsList = map strToBS (splitOn "," origins)
        in policyFromOrigins (Just ((originsList, True)))

getCorsPolicy :: Maybe String -> (Request -> Maybe CorsResourcePolicy)
getCorsPolicy maybeOrigins = policy where
    policy :: Request -> Maybe CorsResourcePolicy
    policy _ = Just (generateCorsPolicy (maybeOrigins))

-- TODO: Add encompasing middleware that checks the Origin req header
-- See: https://hackage.haskell.org/package/wai-cors-0.2.5/docs/Network-Wai-Middleware-Cors.html#v:cors
-- Check that the value of the Origin header matches it's expectations.
