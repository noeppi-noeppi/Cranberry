module Cranberry.Auth (AuthConfig (..), AuthBackend, authSetup, authListMethods, authLdap, OpenIdRedirect (..), OpenIdReturn (..), authOpenIdInit, authOpenId) where

import Cranberry.Types
import Cranberry.Auth.Ldap
import Cranberry.Auth.OpenId

data AuthConfig = AuthConfig {
  anonymous :: PermissionLevel,
  ldap :: LdapConfig,
  oidc :: OpenIdConfig
} deriving (Show, Generic)

instance Configuration AuthConfig where
  defaultConfiguration = AuthConfig {
    anonymous = NoPermission,
    ldap = defaultConfiguration,
    oidc = defaultConfiguration
  }

authSetup :: StorageAdapter db => AuthConfig -> db -> URL -> IO (AuthBackend db)
authSetup config db oidcRedirect = do
  subSystemLdap <- ldapSetup (ldap config)
  subSystemOpenId <- openIdSetup (oidc config) oidcRedirect
  pure (AuthBackend (anonymous config) db subSystemLdap subSystemOpenId)

authListMethods :: StorageAdapter db => AuthBackend db -> [String]
authListMethods backend = ["login" | ldapIsEnabled (subSystemLdap backend)] ++ ["oidc" | openIdIsEnabled (subSystemOpenId backend)]

authLdap :: StorageAdapter db => AuthBackend db -> (String, String) -> IO AuthenticationResult
authLdap backend credentials = ldapLogin (subSystemLdap backend) (authTokenStorage backend) credentials

authOpenIdInit :: StorageAdapter db => AuthBackend db -> IO (Maybe OpenIdRedirect)
authOpenIdInit backend = openIdRedirect (subSystemOpenId backend)

authOpenId :: StorageAdapter db => AuthBackend db -> OpenIdReturn -> IO AuthenticationResult
authOpenId backend ret = openIdLogin (subSystemOpenId backend) (authTokenStorage backend) ret

data StorageAdapter db => AuthBackend db = AuthBackend {
  authDefaultPermission :: PermissionLevel,
  authTokenStorage :: db,
  subSystemLdap :: LdapAuthenticator,
  subSystemOpenId :: OpenIdAuthenticator
}

instance StorageAdapter db => Disposable (AuthBackend db) where
  dispose backend = dispose (authTokenStorage backend)

instance StorageAdapter db => Authenticator (AuthBackend db) where
  authenticate :: StorageAdapter db => AuthBackend db -> UserCredentials -> IO AuthenticationResult
  authenticate backend Anonymous = pure (Success $ UserPrincipal { userId = Nothing, userPermissionLevel = authDefaultPermission backend, userAccessToken = Nothing })
  authenticate backend (Token token) = getAccessTokenDetails (authTokenStorage backend) token >>= \case
    Nothing -> pure InvalidCredentials
    Just (username, permissionLevel) -> pure (Success $ UserPrincipal { userId = Just username, userPermissionLevel = permissionLevel, userAccessToken = Just token })
