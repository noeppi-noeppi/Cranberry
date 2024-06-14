module Cranberry.Types (
  module Cranberry.Types,
  module Data.Maybe,
  module Data.List,
  module Data.Int,
  module Data.String,
  module GHC.Generics) where

import Data.Maybe
import Data.List
import Data.Int
import Data.String
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import GHC.Generics

validShortLink :: String -> Bool
validShortLink string = not (null string) && string /= "_" && all validChar string
  where validChar :: Char -> Bool
        validChar char = C.isLetter char || C.isDigit char || char == '_' || char =='-' || char == '.'

validShortLinkWildcard :: String -> Bool
validShortLinkWildcard string = not (null string) && (validShortLink string || validShortLink (init string) && '*' == last string)

validRedirectUrl :: URL -> Bool
validRedirectUrl (URL url) = T.pack "http:/" `T.isPrefixOf` lowerUrl || T.pack "https:/" `T.isPrefixOf` lowerUrl
  where lowerUrl = T.toLower url

newtype URL = URL T.Text deriving (Eq, Show)
resolveOnBaseUrl :: URL -> T.Text -> URL
resolveOnBaseUrl (URL base) rel
  | T.null rel                     = URL base
  | T.pack "/" `T.isPrefixOf` rel  = resolveOnBaseUrl (URL base) $ T.tail rel
  | T.pack "/" `T.isSuffixOf` base = URL $ base `T.append` rel
  | otherwise                      = URL $ base `T.append` T.pack "/" `T.append` rel
instance Aeson.FromJSON URL where
  parseJSON json = URL <$> (Aeson.parseJSON json :: Aeson.Types.Parser T.Text)
instance Aeson.ToJSON URL where
  toJSON (URL url) = Aeson.toJSON url

class (Show a, Generic a) => Configuration a where
  defaultConfiguration :: a

class Disposable a where
  dispose :: a -> IO ()
  dispose = mempty

class Disposable a => StorageAdapter a where
  getShortLink :: a -> String -> IO (Maybe URL)
  putShortLink :: a -> String -> URL -> IO ()
  putNewShortLink :: a -> String -> URL -> IO Bool
  putRandomShortLink :: a -> URL -> IO String
  deleteShortLink :: a -> String -> IO ()
  listShortLinks :: a -> IO (Map.Map String URL)
  getUserForAccessToken :: a -> String -> IO (Maybe String)
  createAccessToken :: a -> String -> IO String
  revokeAccessToken :: a -> String -> IO ()

data Permission = CreateAnonymousShortLinks | CreateNamedShortLinks | ManageShortLinks deriving (Eq, Ord, Show)
data UserCredentials = Anonymous | Login { loginUsername :: String, loginPassword :: String } | Token { accessToken :: String } deriving Eq
data AuthenticationResult = Success UserPrincipal | InvalidCredentials | ServiceUnavailable deriving Show
data UserPrincipal = UserPrincipal {
  userId :: Maybe String,
  userPermissions :: Set.Set Permission,
  userAccessToken :: Maybe String
} deriving Show

hasPermission :: UserPrincipal -> Permission -> Bool
hasPermission user permission = Set.member permission (userPermissions user)

class Disposable a => Authenticator a where
  authenticate :: a -> UserCredentials -> IO AuthenticationResult

data MessageContent a = MessageContent String a
