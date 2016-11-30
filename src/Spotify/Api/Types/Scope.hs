module Spotify.Api.Types.Scope where

import           Data.Aeson       (FromJSON (parseJSON), ToJSON, toJSON,
                                   withArray)
import           Data.Aeson.Types (Array, Parser)
import           Data.Text        (Text, pack, unpack)
import qualified Data.Vector      as V
import           Servant          (ToHttpApiData (..), toQueryParam)

-- | If no scope is specified, access is permitted only to publicly available
--   information: that is, only information normally visible to normal
--   logged-in users of the Spotify desktop, web, and mobile clients
--   (e.g. public playlists).
data Scope = Scope
    { getScopes :: [Text]
    }

instance Show Scope where
    show s = unwords $ map unpack (getScopes s)

instance ToHttpApiData Scope where
    toQueryParam = pack . show

instance FromJSON Scope where
    parseJSON = withArray "Scope" $ \scopes -> do
        parsedScopes <- parseScopes scopes
        return Scope { getScopes = parsedScopes } where
            parseScopes :: Array -> Parser [Text]
            parseScopes = mapM parseJSON . V.toList

instance ToJSON Scope where
    toJSON = toJSON . show

scopeFromList :: [Text] -> Scope
scopeFromList scopes = Scope { getScopes = scopes }

-- Read access to user's private playlists.
playlistReadPrivate :: String
playlistReadPrivate = "playlist-read-private"

-- Include collaborative playlists when requesting a user's playlists.
playlistReadCollaborative :: String
playlistReadCollaborative = "playlist-read-collaborative"

-- Write access to a user's public playlists.
playlistModifyPublic :: String
playlistModifyPublic = "playlist-modify-public"

-- Write access to a user's private playlists.
playlistModifyPrivate :: String
playlistModifyPrivate = "playlist-modify-private"

-- Control playback of a Spotify track. This scope is currently only available
-- to Spotify native SDKs (for example, the iOS SDK and the Android SDK). The
-- user must have a Spotify Premium account.
streaming :: String
streaming = "streaming"

-- Write/delete access to the list of artists and other users that the user follows.
userFollowModify :: String
userFollowModify = "user-follow-modify"

-- Read access to the list of artists and other users that the user follows.
userFollowRead :: String
userFollowRead = "user-follow-read"

-- Read access to a user's 'Your Music' library.
userLibraryRead :: String
userLibraryRead = "user-library-read"

-- Write/delete access to a user's 'Your Music' library.
userLibraryModify :: String
userLibraryModify = "user-library-modify"

-- Read access to user's subscription details (type of user account).
userReadPrivate :: String
userReadPrivate = "user-read-private"

-- Read access to the user's birthdate.
userReadBirthdate :: String
userReadBirthdate = "user-read-birthdate"

-- Read access to user's email address.
userReadEmail :: String
userReadEmail = "user-read-email"

-- Read access to a user's top artists and tracks
userTopRead :: String
userTopRead = "user-top-read"
