module Cranberry.Auth where

import Cranberry.Types
import Cranberry.LdapParser (parseLdapFilter)
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
  permissions :: LdapPermissionConfig
} deriving (Show, Generic)

data LdapAttributeConfig = LdapAttributeConfig {
  uid :: String,
  membership :: String
} deriving (Show, Generic)

data LdapPermissionConfig = LdapPermissionConfig {
  anonymous :: String,
  groups :: LdapGroupSyncConfig
} deriving (Show, Generic)

data LdapGroupSyncConfig = LdapGroupSyncConfig {
  createAnonymous :: String,
  createNamed :: String,
  manage :: String
} deriving (Show, Generic)

instance Configuration LdapConfig where
  defaultConfiguration = LdapConfig {
    enabled = False,
    host ="localhost",
    tls = False,
    port = 389,
    bindDn = Nothing,
    bindPassword = Nothing,
    baseDn = "",
    userSearch = "(objectClass=person)",
    attributes = defaultConfiguration,
    permissions = defaultConfiguration
  }

instance Configuration LdapAttributeConfig where
  defaultConfiguration = LdapAttributeConfig {
    uid = "uid",
    membership = "memberOf"
  }

instance Configuration LdapPermissionConfig where
  defaultConfiguration = LdapPermissionConfig {
    anonymous = "none",
    groups = defaultConfiguration
  }

instance Configuration LdapGroupSyncConfig where
  defaultConfiguration = LdapGroupSyncConfig {
    createAnonymous = "",
    createNamed = "",
    manage = ""
  }

data StorageAdapter db => LdapAuthenticator db = LdapDisabled | LdapEnabled {
  ldapHost :: Ldap.Host,
  ldapPort :: Ldap.PortNumber,
  ldapBindDn :: Ldap.Dn,
  ldapBindPassword :: Ldap.Password,
  ldapBaseDn :: Ldap.Dn,
  ldapSearchFilter :: Ldap.Filter,
  ldapUidAttribute :: Ldap.Attr,
  ldapMembershipAttribute :: Ldap.Attr,
  ldapAnonymousPermissions :: Set.Set Permission,
  ldapGroups :: LdapGroupSyncConfig,
  ldapTokenStorage :: db
}

connectAuth :: StorageAdapter db => LdapConfig -> db -> IO (LdapAuthenticator db)
connectAuth config db = if not $ enabled config
  then return LdapDisabled
  else do
    searchFilter <- case parseLdapFilter $ userSearch config of
      Just filter -> return filter
      Nothing -> throw InvalidLdapFilter
    anonymousPermissions <- case anonymous $ permissions config of
      "none" -> return Set.empty
      "create_anonymous" -> return createAnonPermissions
      "create_named" -> return createNamedPermissions
      "manage" -> return managePermissions
      wrong -> throw $ InvalidPermission wrong
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
      ldapAnonymousPermissions = anonymousPermissions,
      ldapGroups = groups $ permissions config,
      ldapTokenStorage = db}

managePermissions :: Set.Set Permission
managePermissions = Set.fromList [CreateAnonymousShortLinks, CreateNamedShortLinks, ManageShortLinks]
createNamedPermissions :: Set.Set Permission
createNamedPermissions = Set.fromList [CreateAnonymousShortLinks, CreateNamedShortLinks]
createAnonPermissions :: Set.Set Permission
createAnonPermissions = Set.fromList [CreateAnonymousShortLinks]

instance StorageAdapter db => Disposable (LdapAuthenticator db)
instance StorageAdapter db => Authenticator (LdapAuthenticator db) where
  authenticate LdapDisabled Anonymous = return $ Success $ UserPrincipal {
    userId = Nothing, userPermissions = createNamedPermissions, userAccessToken = Nothing}
  authenticate LdapDisabled (Login username _) = return InvalidCredentials
  authenticate LdapDisabled (Token _) = return InvalidCredentials
  authenticate con@LdapEnabled { } Anonymous = return $ Success $ UserPrincipal {
    userId = Nothing, userPermissions = ldapAnonymousPermissions con, userAccessToken = Nothing}
  authenticate con@LdapEnabled { } credentials = do
    res <- Ldap.with (ldapHost con) (ldapPort con) ldapTransaction
    case res of
      Left err -> putStrLn "LDAP error." >> print err >> return ServiceUnavailable
      Right Nothing -> return InvalidCredentials
      Right (Just user) -> return $ Success user
    where ldapTransaction :: Ldap.Ldap -> IO (Maybe UserPrincipal)
          ldapTransaction ldap = case credentials of
            Login username password -> ldapTransactionUser ldap username password
            Token token -> ldapTransactionToken ldap token

          ldapTransactionUser :: Ldap.Ldap -> String -> String -> IO (Maybe UserPrincipal)
          ldapTransactionUser ldap username password = do
            _ <- Ldap.bind ldap (ldapBindDn con) (ldapBindPassword con)
            search <- ldapSearchFor ldap username
            case search of
              Just entry -> ldapBindUser ldap username password entry
              Nothing -> return Nothing

          ldapTransactionToken :: Ldap.Ldap -> String -> IO (Maybe UserPrincipal)
          ldapTransactionToken ldap token = do
            usernameQuery <- getUserForAccessToken (ldapTokenStorage con) token
            case usernameQuery of
              Just username -> do
                _ <- Ldap.bind ldap (ldapBindDn con) (ldapBindPassword con)
                search <- ldapSearchFor ldap username
                case search of
                  Just entry -> return $ Just $ ldapConstructUser username entry (Just token)
                  Nothing -> return Nothing 
              Nothing -> return Nothing
 
          ldapUidFilter :: String -> Ldap.Filter
          ldapUidFilter username = ldapUidAttribute con Ldap.:= (TE.encodeUtf8 . T.pack $ username)
          ldapFullSearchFilter :: String -> Ldap.Filter
          ldapFullSearchFilter username = Ldap.And (ldapSearchFilter con :| [ldapUidFilter username])
          ldapSearchOptions :: Ldap.Search.Mod Ldap.Search
          ldapSearchOptions = Ldap.scope Ldap.WholeSubtree <> Ldap.derefAliases Ldap.DerefAlways
          ldapSearchAttributes :: [Ldap.Attr]
          ldapSearchAttributes = [ldapUidAttribute con, ldapMembershipAttribute con]

          ldapSearchFor :: Ldap.Ldap -> String -> IO (Maybe Ldap.SearchEntry)
          ldapSearchFor ldap username = do
            search <- Ldap.search ldap (ldapBaseDn con) ldapSearchOptions (ldapFullSearchFilter username) ldapSearchAttributes
            case search of
              [] -> return Nothing
              [entry] -> return $ Just entry
              _ -> putStrLn ("Non-unique LDAP qery for uid " ++ username) >> return Nothing

          ldapBindUser :: Ldap.Ldap -> String -> String -> Ldap.SearchEntry -> IO (Maybe UserPrincipal)
          ldapBindUser ldap username password search@(Ldap.SearchEntry userDn userAttrs) = do
            bindResult <- Ldap.Bind.bindEither ldap userDn (Ldap.Password . TE.encodeUtf8 . T.pack $ password)
            case bindResult of
              Left err -> return Nothing
              Right () -> return $ Just $ ldapConstructUser username search Nothing

          ldapConstructUser :: String -> Ldap.SearchEntry -> Maybe String -> UserPrincipal
          ldapConstructUser username (Ldap.SearchEntry _ userAttrs) maybeAccessToken = UserPrincipal {
            userId = Just userId, userPermissions = permissions, userAccessToken = maybeAccessToken}
            where decode :: Ldap.AttrValue -> String
                  decode bytes = T.unpack $ TE.decodeUtf8Lenient bytes
                  userId :: String
                  userId = maybe username decode $ listToMaybe $ concatMap snd $ find ((ldapUidAttribute con ==) . fst) userAttrs
                  groups :: [String]
                  groups = map decode $ concatMap snd $ find ((ldapMembershipAttribute con ==) . fst) userAttrs
                  groupPermissions :: String -> Set.Set Permission
                  groupPermissions group = Set.unions [
                    if createAnonymous (ldapGroups con) == group then createAnonPermissions else Set.empty,
                    if createNamed (ldapGroups con) == group then createNamedPermissions else Set.empty,
                    if manage (ldapGroups con) == group then managePermissions else Set.empty]
                  permissions :: Set.Set Permission
                  permissions = Set.unions $ map groupPermissions groups
