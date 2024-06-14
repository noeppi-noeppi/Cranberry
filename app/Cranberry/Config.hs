module Cranberry.Config where

import System.IO
import System.Directory
import GHC.Generics
import Conferer.Config
import Conferer.FromConfig
import Conferer.Source.Yaml

import Cranberry.Types
import Cranberry.Webserver (WebserverConfig)
import Cranberry.Database (PostgresConfig)
import Cranberry.Auth (LdapConfig, LdapAttributeConfig, LdapPermissionConfig, LdapGroupSyncConfig)

data AppConfig = AppConfig {
  server :: WebserverConfig,
  database :: PostgresConfig,
  ldap :: LdapConfig
} deriving (Show, Generic)

instance FromConfig AppConfig
instance FromConfig WebserverConfig
instance FromConfig PostgresConfig
instance FromConfig LdapConfig
instance FromConfig LdapAttributeConfig
instance FromConfig LdapPermissionConfig
instance FromConfig LdapGroupSyncConfig

instance Configuration AppConfig where
  defaultConfiguration = AppConfig {
    server = defaultConfiguration,
    database = defaultConfiguration,
    ldap = defaultConfiguration
  }

loadAppConfig :: FilePath -> IO AppConfig
loadAppConfig path = do
  configFileExists <- doesFileExist path
  config <- if configFileExists
    then addSource (fromFilePath path) emptyConfig
    else return emptyConfig
  fetchFromRootConfigWithDefault config defaultConfiguration
