{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Envelope
    ( Envelope
    , success
    , error'
    , fromServantError
    ) where

import           Data.Aeson              (ToJSON, encode)
import           Data.Text               (Text, pack, replace, toUpper)
import           Data.Text.Lazy          (toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Servant                 (ServantErr (ServantErr, errBody, errHTTPCode, errHeaders, errReasonPhrase))
import           Web.Envelope            (toErrEnvelope, toSuccessEnvelope)
import qualified Web.Envelope            as E

import           Utils                   (jsonContentType)

type Envelope a = E.Envelope Text a

success :: ToJSON a => a -> Envelope a
success = toSuccessEnvelope

-- errorCode, message
error' :: ToJSON a => Text -> Text -> Envelope a
error' = toErrEnvelope

fromServantError :: ServantErr -> ServantErr
fromServantError servantError =
    ServantErr
        { errHTTPCode = errHTTPCode servantError
        , errReasonPhrase = errReasonPhrase servantError
        , errBody = errorEnvelope
        , errHeaders = errHeaders servantError ++ [jsonContentType]
        } where
        reasonPhrase = pack $ errReasonPhrase servantError
        errorCode = replace " " "_" . toUpper $ reasonPhrase
        message = toStrict . decodeUtf8 $ errBody servantError
        errorEnvelope = encode (error' errorCode message :: Envelope Text)
