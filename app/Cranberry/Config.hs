module Cranberry.Config (AppConfig (..), loadAppConfig) where

import System.IO
import System.Directory
import GHC.Generics
import Conferer.Config
import Conferer.FromConfig
import Conferer.Source.Yaml hiding (fromConfig)

import Cranberry.Types
import Cranberry.Webserver (WebserverConfig)
import Cranberry.Database (PostgresConfig)
import Cranberry.Auth (AuthConfig)
import Cranberry.Auth.Ldap (LdapConfig, LdapAttributeConfig, LdapGroupSyncConfig)
import Cranberry.Auth.OpenId (OpenIdConfig, OpenIdRoleSyncConfig)

data AppConfig = AppConfig {
  server :: WebserverConfig,
  database :: PostgresConfig,
  auth :: AuthConfig
} deriving (Show, Generic)

instance FromConfig PermissionLevel where
  fromConfig key cnf = do
    str <- fromConfig key cnf :: IO String
    readPermissionLevel str

instance FromConfig AppConfig
instance FromConfig WebserverConfig
instance FromConfig PostgresConfig
instance FromConfig AuthConfig
instance FromConfig LdapConfig
instance FromConfig LdapAttributeConfig
instance FromConfig LdapGroupSyncConfig
instance FromConfig OpenIdConfig
instance FromConfig OpenIdRoleSyncConfig

instance Configuration AppConfig where
  defaultConfiguration = AppConfig {
    server = defaultConfiguration,
    database = defaultConfiguration,
    auth = defaultConfiguration
  }

loadAppConfig :: FilePath -> IO AppConfig
loadAppConfig path = do
  configFileExists <- doesFileExist path
  config <- if configFileExists
    then addSource (fromFilePath path) emptyConfig
    else return emptyConfig
  fetchFromRootConfigWithDefault config defaultConfiguration
