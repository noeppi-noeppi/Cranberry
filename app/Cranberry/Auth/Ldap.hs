module Cranberry.Auth.Ldap (LdapConfig (..), LdapAttributeConfig (..), LdapGroupSyncConfig (..), LdapAuthenticator, ldapSetup, ldapLogin, ldapIsEnabled) where

import Cranberry.Types
import Cranberry.Auth.LdapParser (parseLdapFilter)
import Control.Exception
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Ldap.Client as Ldap
import qualified Ldap.Client.Bind as Ldap.Bind
import qualified Ldap.Client.Search as Ldap.Search

data InvalidLdapFilter = InvalidLdapFilter deriving Show
instance Exception InvalidLdapFilter

newtype InvalidPermission = InvalidPermission String deriving Show
instance Exception InvalidPermission

data LdapConfig = LdapConfig {
  enabled :: Bool,
  host :: String,
  tls :: Bool,
  port :: Int,
  bindDn :: Maybe String,
  bindPassword :: Maybe String,
  baseDn :: String,
  userSearch :: String,
  attributes :: LdapAttributeConfig,
  defaultPermission :: PermissionLevel,
  groups :: LdapGroupSyncConfig
} deriving (Show, Generic)

data LdapAttributeConfig = LdapAttributeConfig {
  uid :: String,
  membership :: String
} deriving (Show, Generic)

data LdapGroupSyncConfig = LdapGroupSyncConfig {
  createAnonymous :: String,
  createNamed :: String,
  manage :: String
} deriving (Show, Generic)

instance Configuration LdapConfig where
  defaultConfiguration = LdapConfig {
    enabled = False,
    host = "localhost",
    tls = False,
    port = 389,
    bindDn = Nothing,
    bindPassword = Nothing,
    baseDn = "",
    userSearch = "(objectClass=person)",
    attributes = defaultConfiguration,
    defaultPermission = CreateAnonymousShortLinks,
    groups = defaultConfiguration
  }

instance Configuration LdapAttributeConfig where
  defaultConfiguration = LdapAttributeConfig {
    uid = "uid",
    membership = "memberOf"
  }

instance Configuration LdapGroupSyncConfig where
  defaultConfiguration = LdapGroupSyncConfig {
    createAnonymous = "",
    createNamed = "",
    manage = ""
  }

data LdapAuthenticator = LdapDisabled | LdapEnabled {
  ldapHost :: Ldap.Host,
  ldapPort :: Ldap.PortNumber,
  ldapBindDn :: Ldap.Dn,
  ldapBindPassword :: Ldap.Password,
  ldapBaseDn :: Ldap.Dn,
  ldapSearchFilter :: Ldap.Filter,
  ldapUidAttribute :: Ldap.Attr,
  ldapMembershipAttribute :: Ldap.Attr,
  ldapAnonymousPermissionLevel :: PermissionLevel,
  ldapGroups :: LdapGroupSyncConfig
}

ldapSetup :: LdapConfig -> IO LdapAuthenticator
ldapSetup config = if not $ enabled config
  then return LdapDisabled
  else do
    searchFilter <- case parseLdapFilter $ userSearch config of
      Just filter -> return filter
      Nothing -> throw InvalidLdapFilter
    return $ LdapEnabled {
      ldapHost = if tls config
        then Ldap.Tls (host config) Ldap.defaultTlsSettings
        else Ldap.Plain $ host config,
      ldapPort = fromIntegral $ port config,
      ldapBindDn = Ldap.Dn $ maybe T.empty T.pack $ bindDn config,
      ldapBindPassword = Ldap.Password $ TE.encodeUtf8 $ maybe T.empty T.pack $ bindPassword config,
      ldapBaseDn = Ldap.Dn $ T.pack $ baseDn config,
      ldapSearchFilter = searchFilter,
      ldapUidAttribute = Ldap.Attr $ T.pack $ uid $ attributes config,
      ldapMembershipAttribute = Ldap.Attr $ T.pack $ membership $ attributes config,
      ldapAnonymousPermissionLevel = defaultPermission config,
      ldapGroups = groups config}

ldapIsEnabled :: LdapAuthenticator -> Bool
ldapIsEnabled LdapDisabled = False
ldapIsEnabled LdapEnabled {} = True

ldapLogin :: StorageAdapter db => LdapAuthenticator -> db -> (String, String) -> IO AuthenticationResult
ldapLogin LdapDisabled _ _ = pure ServiceUnavailable
ldapLogin con db (username, password) = Ldap.with (ldapHost con) (ldapPort con) transaction >>= \case
    Left _ -> pure ServiceUnavailable
    Right result -> pure result
  where transaction :: Ldap.Ldap -> IO AuthenticationResult
        transaction ldap = do
          _ <- Ldap.bind ldap (ldapBindDn con) (ldapBindPassword con)
          search <- ldapSearchFor con ldap username
          case search of
            Just entry -> ldapBindUser con db ldap (username, password) entry >>= \case
              Just user -> pure (Success user)
              Nothing -> pure InvalidCredentials
            Nothing -> pure InvalidCredentials

ldapSearchFor :: LdapAuthenticator -> Ldap.Ldap -> String -> IO (Maybe Ldap.SearchEntry)
ldapSearchFor LdapDisabled _ _ = pure Nothing
ldapSearchFor con ldap username = do
  search <- Ldap.search ldap (ldapBaseDn con) ldapSearchOptions (ldapFullSearchFilter username) ldapSearchAttributes
  case search of
    [] -> return Nothing
    [entry] -> return $ Just entry
    _ -> putStrLn ("Non-unique LDAP query for uid " ++ username) >> return Nothing
  where ldapUidFilter :: String -> Ldap.Filter
        ldapUidFilter username = ldapUidAttribute con Ldap.:= (TE.encodeUtf8 . T.pack $ username)
        ldapFullSearchFilter :: String -> Ldap.Filter
        ldapFullSearchFilter username = Ldap.And (ldapSearchFilter con :| [ldapUidFilter username])
        ldapSearchOptions :: Ldap.Search.Mod Ldap.Search
        ldapSearchOptions = Ldap.scope Ldap.WholeSubtree <> Ldap.derefAliases Ldap.DerefAlways
        ldapSearchAttributes :: [Ldap.Attr]
        ldapSearchAttributes = [ldapUidAttribute con, ldapMembershipAttribute con]

ldapBindUser :: StorageAdapter db => LdapAuthenticator -> db -> Ldap.Ldap -> (String, String) -> Ldap.SearchEntry -> IO (Maybe UserPrincipal)
ldapBindUser LdapDisabled _ _ _ _ = pure Nothing
ldapBindUser con db ldap (username, password) search@(Ldap.SearchEntry userDn userAttrs) = do
  bindResult <- Ldap.Bind.bindEither ldap userDn (Ldap.Password . TE.encodeUtf8 . T.pack $ password)
  case bindResult of
    Left err -> return Nothing
    Right () -> Just <$> ldapConstructUser username search
  where ldapConstructUser :: String -> Ldap.SearchEntry -> IO UserPrincipal
        ldapConstructUser username (Ldap.SearchEntry _ userAttrs) = do
          accessToken <- createAccessToken db username permissionLevel
          pure UserPrincipal { userId = Just userId, userPermissionLevel = permissionLevel, userAccessToken = Just accessToken}
          where decode :: Ldap.AttrValue -> String
                decode bytes = T.unpack $ TE.decodeUtf8Lenient bytes
                userId :: String
                userId = maybe username decode $ listToMaybe $ concatMap snd $ find ((ldapUidAttribute con ==) . fst) userAttrs
                groups :: [String]
                groups = map decode $ concatMap snd $ find ((ldapMembershipAttribute con ==) . fst) userAttrs
                groupPermissionLevel :: String -> PermissionLevel
                groupPermissionLevel group = highestPermissionLevel [
                  if createAnonymous (ldapGroups con) == group then CreateAnonymousShortLinks else NoPermission,
                  if createNamed (ldapGroups con) == group then CreateNamedShortLinks else NoPermission,
                  if manage (ldapGroups con) == group then ManageShortLinks else NoPermission]
                permissionLevel :: PermissionLevel
                permissionLevel = highestPermissionLevel $ map groupPermissionLevel groups
