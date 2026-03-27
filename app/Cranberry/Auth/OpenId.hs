module Cranberry.Auth.OpenId (OpenIdConfig (..), OpenIdRoleSyncConfig (..), OpenIdAuthenticator, OpenIdRedirect (..), OpenIdReturn (..), openIdSetup, openIdIsEnabled, openIdIsAutoLogin, openIdRedirect, openIdLogin) where

import Cranberry.Types
import Control.Exception
import Control.Lens ((^.))
import Data.Map (Map)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time.Clock (getCurrentTime)
import Web.Cookie (setCookieValue)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.URI as URI
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Data.Aeson as Aeson
import qualified Crypto.JWT as JWT
import qualified OpenID.Connect.Authentication as OIDC
import qualified OpenID.Connect.TokenResponse as OIDC
import qualified OpenID.Connect.Client.Provider as OIDC
import qualified OpenID.Connect.Client.Flow.AuthorizationCode as OIDC

data InvalidOpenIdURI = InvalidOpenIdURI deriving Show
instance Exception InvalidOpenIdURI

data InvalidOpenIdDiscovery = InvalidOpenIdDiscovery deriving Show
instance Exception InvalidOpenIdDiscovery

data OpenIdConfig = OpenIdConfig {
  enabled :: Bool,
  autoLogin :: Bool,
  discovery :: String,
  clientId :: String,
  clientSecret :: String,
  defaultPermission :: PermissionLevel,
  roles :: OpenIdRoleSyncConfig
} deriving (Show, Generic)

data OpenIdRoleSyncConfig = OpenIdRoleSyncConfig {
  login :: String,
  createAnonymous :: String,
  createNamed :: String,
  manage :: String
} deriving (Show, Generic)

instance Configuration OpenIdConfig where
  defaultConfiguration = OpenIdConfig {
    enabled = False,
    autoLogin = False,
    discovery = "",
    clientId = "",
    clientSecret = "",
    defaultPermission = CreateAnonymousShortLinks,
    roles = defaultConfiguration
  }

instance Configuration OpenIdRoleSyncConfig where
  defaultConfiguration = OpenIdRoleSyncConfig {
    login = "",
    createAnonymous = "",
    createNamed = "",
    manage = ""
  }

data OpenIdAuthenticator = OpenIdDisabled | OpenIdEnabled {
  oidcAutoLogin :: Bool,
  oidcProvider :: OIDC.Provider,
  oidcCredentials :: OIDC.Credentials,
  oidcHTTPS :: OIDC.HTTPS IO,
  oidcDefaultPermissionLevel :: PermissionLevel,
  oidcLoginRole :: Maybe String,
  oidcCreateAnonymousRole :: Maybe String,
  oidcCreateNamedRole :: Maybe String,
  oidcManageRole :: Maybe String
}

openIdSetup :: OpenIdConfig -> URL -> IO OpenIdAuthenticator
openIdSetup config redirectURL = if not $ enabled config
  then return OpenIdDisabled
  else do
    httpsManager <- HTTP.newTlsManager
    let httpsClient = (\request -> HTTP.httpLbs request httpsManager) :: OIDC.HTTPS IO
    discoveryURI <- parseURI' (discovery config)
    discoveryData <- OIDC.discovery httpsClient discoveryURI >>= \case
      Left err -> putStrLn "Failed to fetch OpenID discovery" >> print err >> throw InvalidOpenIdDiscovery
      Right (dat, _) -> pure dat
    jwkData <- OIDC.keysFromDiscovery httpsClient discoveryData >>= \case
      Left err -> putStrLn "Failed to fetch OpenID JWKs" >> print err >> throw InvalidOpenIdDiscovery
      Right (keys, _) -> pure keys
    redirectURI <- parseURI redirectURL
    pure OpenIdEnabled {
      oidcAutoLogin = autoLogin config,
      oidcProvider = OIDC.Provider {
        OIDC.providerDiscovery = discoveryData,
        OIDC.providerKeys = jwkData
      },
      oidcCredentials = OIDC.Credentials {
        OIDC.assignedClientId = T.pack (clientId config),
        OIDC.clientSecret = OIDC.AssignedSecretText (T.pack $ clientSecret config),
        OIDC.clientRedirectUri = redirectURI
      },
      oidcHTTPS = httpsClient,
      oidcDefaultPermissionLevel = defaultPermission config,
      oidcLoginRole = if null (login $ roles config) then Nothing else Just (login $ roles config),
      oidcCreateAnonymousRole = if null (createAnonymous $ roles config) then Nothing else Just (createAnonymous $ roles config),
      oidcCreateNamedRole = if null (createNamed $ roles config) then Nothing else Just (createNamed $ roles config),
      oidcManageRole = if null (manage $ roles config) then Nothing else Just (manage $ roles config)
    }
  where parseURI :: URL -> IO URI.URI
        parseURI (URL text) = parseURI' (T.unpack text)
        parseURI' :: String -> IO URI.URI
        parseURI' string = case URI.parseURI string of
          Just uri -> pure uri
          Nothing -> throw InvalidOpenIdURI

data OpenIdRedirect = OpenIdRedirect {
  openIdRedirectURL :: URL,
  openIdRedirectCookie :: ByteString
}

data OpenIdReturn = OpenIdReturn {
  openIdReturnCookie :: ByteString,
  openIdReturnCode :: ByteString,
  openIdReturnState :: ByteString
}

openIdIsEnabled :: OpenIdAuthenticator -> Bool
openIdIsEnabled OpenIdDisabled = False
openIdIsEnabled OpenIdEnabled {} = True

openIdIsAutoLogin :: OpenIdAuthenticator -> Bool
openIdIsAutoLogin OpenIdEnabled {} = True
openIdIsAutoLogin OpenIdDisabled = False

openIdRedirect :: OpenIdAuthenticator -> IO (Maybe OpenIdRedirect)
openIdRedirect OpenIdDisabled = pure Nothing
openIdRedirect con = do
  let discoveryData = (OIDC.providerDiscovery $ oidcProvider con) :: OIDC.Discovery
  let authRequest = (OIDC.defaultAuthenticationRequest (OIDC.openid <> OIDC.email) $ oidcCredentials con) :: OIDC.AuthenticationRequest
  OIDC.authenticationRedirect discoveryData authRequest >>= \case
    Left err -> putStrLn "OIDC Authorization flow issue" >> print err >> pure Nothing
    Right (OIDC.RedirectTo uri cookieFunc) -> do
      let url = URL (T.pack $ URI.uriToString id uri "")
      let cookie = setCookieValue (cookieFunc "") :: ByteString
      pure (Just $ OpenIdRedirect url cookie)

openIdLogin :: StorageAdapter db => OpenIdAuthenticator -> db -> OpenIdReturn -> IO AuthenticationResult
openIdLogin OpenIdDisabled _ _ = pure ServiceUnavailable
openIdLogin con db ret = do
  let redirReturn = OIDC.UserReturnFromRedirect {
    OIDC.afterRedirectSessionCookie = openIdReturnCookie ret,
    OIDC.afterRedirectCodeParam = openIdReturnCode ret,
    OIDC.afterRedirectStateParam = openIdReturnState ret
  }
  time <- getCurrentTime
  OIDC.authenticationSuccess (oidcHTTPS con) time (oidcProvider con) (oidcCredentials con) redirReturn >>= \case
    Left err -> pure InvalidCredentials
    Right tokenResponse -> do
      let claims = (OIDC.idToken tokenResponse ^. JWT.unregisteredClaims) :: Map Text Aeson.Value
      let prefUserName = lookupClaim claims "preferred_username" "" :: String
      let email = lookupClaim claims "email" "" :: String
      let clientRoles = lookupClaim claims "clientroles" [] :: [String]
      case openIdFindPermissionLevel con clientRoles of
        Nothing -> pure InvalidCredentials
        Just permissionLevel -> case (prefUserName, email) of
          ("", "") -> pure ServiceUnavailable
          ("", username) -> openIdSetupAccessToken db username permissionLevel
          (username, _) -> openIdSetupAccessToken db username permissionLevel
      where lookupClaim :: Aeson.FromJSON a => Map Text Aeson.Value -> Text -> a -> a
            lookupClaim claims name dfl = case M.lookup name claims of
              Nothing -> dfl
              Just json -> case Aeson.fromJSON json of
                Aeson.Error _ -> dfl
                Aeson.Success value -> value

openIdFindPermissionLevel :: OpenIdAuthenticator -> [String] -> Maybe PermissionLevel
openIdFindPermissionLevel OpenIdDisabled _ = Nothing
openIdFindPermissionLevel con roles = if hasLoginRole then Just (highestPermissionLevel assignedPermissionLevels) else Nothing
  where hasRole :: Maybe String -> Bool -> Bool
        hasRole Nothing dfl = dfl
        hasRole (Just role) _ = role `elem` roles
        hasLoginRole = hasRole (oidcLoginRole con) True
        hasAnonRole = hasRole (oidcCreateAnonymousRole con) False
        hasNamedRole = hasRole (oidcCreateNamedRole con) False
        hasManageRole = hasRole (oidcManageRole con) False
        assignedPermissionLevels :: [PermissionLevel]
        assignedPermissionLevels = [oidcDefaultPermissionLevel con] ++ [CreateAnonymousShortLinks | hasAnonRole] ++ [CreateNamedShortLinks | hasNamedRole] ++ [ManageShortLinks | hasManageRole]

openIdSetupAccessToken :: StorageAdapter db => db -> String -> PermissionLevel -> IO AuthenticationResult
openIdSetupAccessToken db username permissionLevel = do
  accessToken <- createAccessToken db username permissionLevel
  pure (Success $ UserPrincipal { userId = Just username, userPermissionLevel = permissionLevel, userAccessToken = Just accessToken })
