{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Views.Index (render) where

import           Data.ISO3166_CountryCodes                    (CountryCode)
import           Data.Text                                    (Text)
import           Text.Blaze                                   (toMarkup)
import           Text.Blaze.Html
import           Text.Hamlet

import qualified Network.Spotify.Api.Types.Followers          as Followers
import qualified Network.Spotify.Api.Types.Image              as Image
import           Network.Spotify.Api.Types.PlaylistSimplified as Playlist
import qualified Network.Spotify.Api.Types.SpotifyUrl         as Spotify
import qualified Network.Spotify.Api.Types.User               as Spotify

-- viewFile = shamletFile "../../views/index.hamlet"

data IndexRoute = Stylesheet

renderUrl :: Render IndexRoute
renderUrl Stylesheet _ = "/main.css"

-- | The main template
render :: Maybe Spotify.User -> Maybe [Playlist.PlaylistSimplified] ->
    Maybe String -> Html
render maybeUser maybePlaylists maybeError =
    $(hamletFile "views/index.hamlet") renderUrl

instance ToMarkup (Maybe Text) where
    toMarkup txt = case txt of
        Nothing    -> string ""
        Just value -> text value
    preEscapedToMarkup txt = case txt of
        Nothing    -> preEscapedString ""
        Just value -> preEscapedText value

instance ToMarkup (Maybe CountryCode) where
    toMarkup country = case country of
        Nothing    -> string ""
        Just value -> string $ show value
    preEscapedToMarkup country = case country of
        Nothing    -> preEscapedString ""
        Just value -> preEscapedString $ show value
