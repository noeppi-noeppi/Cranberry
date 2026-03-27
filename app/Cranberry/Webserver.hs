module Cranberry.Webserver (launchServer, WebserverConfig (..)) where

import Cranberry.Types
import Cranberry.Auth
import Cranberry.Cranstack
import qualified Cranberry.TemplatePages as Template
import qualified Happstack.Server
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.Aeson as Aeson

data WebserverConfig = WebserverConfig {
  self :: String,
  management :: Maybe String,
  bind :: String,
  port :: Int
} deriving (Show, Generic)

instance Configuration WebserverConfig where
  defaultConfiguration = WebserverConfig {
    self = "http://localhost/",
    management = Nothing,
    bind = "0.0.0.0",
    port = 80
  }

managementSelf :: WebserverConfig -> String
managementSelf config = case management config of
  Just url -> url
  Nothing -> (self config)

launchServer :: StorageAdapter db => WebserverConfig -> db -> AuthConfig -> IO ()
launchServer config db authConfig = do
  let openIdRedirectURL = resolveOnBaseUrl (URL $ T.pack $ managementSelf config) "/_/api/oidc/return" :: URL
  auth <- authSetup authConfig db openIdRedirectURL
  launchServer' config db auth

launchServer' :: StorageAdapter db => WebserverConfig -> db -> AuthBackend db -> IO ()
launchServer' config db auth = do
  socket <- bindIPv4 (bind config) (port config)
  simpleHTTPWithSocket socket serverConfig (route db auth $ URL $ T.pack $ self config)
  where serverConfig :: Conf
        serverConfig = Conf {
          Happstack.Server.port = port config,
          validator = validator nullConf,
          logAccess = logAccess nullConf,
          timeout = timeout nullConf,
          threadGroup = threadGroup nullConf
        }

route :: StorageAdapter db => db -> AuthBackend db -> URL -> ServerPart Response
route db auth baseURL = asum [
  dir "_" control,
  mainPage,
  redirect,
  redirectRecursive,
  errorPage notFound "Not Found"]
  where mainPage :: ServerPart Response
        mainPage = do
          nullDir
          method GET
          found (URL $ T.pack "/_") $ toResponse ()
        redirect :: ServerPart Response
        redirect = path $ \id -> do
          nullDir
          method GET
          guardRq (\_ -> validShortLink id)
          liftSIO (getShortLink db id) $ \dest -> case dest of
            Just url -> seeOther url $ toResponse ()
            Nothing -> errorPage notFound "Not Found"
        redirectRecursive :: ServerPart Response
        redirectRecursive = path $ \id -> do
          method GET
          guardRq (\_ -> validShortLink id)
          liftSIO (getShortLink db $ id ++ "*") $ \dest -> case dest of
            Just url -> uriRest $ \rest -> found (resolveOnBaseUrl url (T.pack rest)) $ toResponse ()
            Nothing -> errorPage notFound "Not Found"
        control :: ServerPart Response
        control = asum [
          dir "index.js" $ serve Template.indexScript,
          dir "style.css" $ serve Template.stylesheet,
          dir "api" api,
          indexPage,
          errorPage notFound "Not found"]
        indexPage = if authAutoOpenId auth
          then do
            nullDir
            method GET
            found (URL $ T.pack "/_/api/oidc") $ toResponse ()
          else serve (Template.indexPage False)
        api :: ServerPart Response
        api = asum [
          dir "me" apiMe,
          dir "login" apiLogin,
          dir "oidc" (asum [apiOidcRedirect, dir "return" (serve $ Template.indexPage True), dir "login" apiOidcLogin]),
          dir "logout" apiLogout,
          dir "create" apiCreate,
          dir "list" apiList,
          dir "revise" apiRevise,
          dir "delete" apiDelete,
          errorPage notFound "Not found"]
        apiMe :: ServerPart Response
        apiMe = withUser auth $ \user -> do
          nullDir
          method GET
          getMe auth user
        apiLogin :: ServerPart Response
        apiLogin = do
          nullDir
          method POST
          withBody $ \body -> do
            let username = T.takeWhile (':' /=) body
            let password =  T.drop 1 $ T.dropWhile (':' /=) body
            liftSIO (authLdap auth (T.unpack username, T.unpack password)) $ \result -> do
              withUser' result $ \user -> getMeToken auth user
        apiOidcRedirect :: ServerPart Response
        apiOidcRedirect = do
          nullDir
          liftSIO (authOpenIdInit auth) $ \case
            Nothing -> errorPage serviceUnavailable "Service Unavailable"
            Just openIdRedir -> do
              addCookie Session (mkCookie "cranberry-login" $ T.unpack $ T.decodeUtf8Lenient $ openIdRedirectCookie openIdRedir) { httpOnly = True, secure = True, sameSite = SameSiteLax }
              found (openIdRedirectURL openIdRedir) $ toResponse ()
        apiOidcLogin :: ServerPart Response
        apiOidcLogin = do
          cookie <- (T.encodeUtf8 . T.pack) <$> lookCookieValue "cranberry-login"
          code <- queryString (lookBS "code")
          state <- queryString (lookBS "state")
          let ret = OpenIdReturn { openIdReturnCookie = cookie, openIdReturnCode = BS.toStrict code, openIdReturnState = BS.toStrict state }
          liftSIO (authOpenId auth ret) $ \result -> do
            withUser' result $ \user -> getMeToken auth user
        apiLogout :: ServerPart Response
        apiLogout = do
          nullDir
          method POST
          withUser auth $ \user -> case userAccessToken user of
            Just token -> liftSIO (revokeAccessToken db token) $ \() -> ok $ toResponse ()
            _ -> errorPage forbidden "Forbidden"
        apiCreate :: ServerPart Response
        apiCreate = asum [
          apiCreateAnon,
          apiCreateNamed]
        apiCreateAnon :: ServerPart Response
        apiCreateAnon = do
          nullDir -- First check is nullDir, so apiCreate propagates through to apiCreateNamed before auth
          method POST
          require auth CreateAnonymousShortLinks $ withBody $ \body -> do
            if validRedirectUrl (URL body)
              then liftSIO (putRandomShortLink db $ URL body) $ \id -> ok $ toResponse $ resolveOnBaseUrl baseURL $ T.pack id
              else errorPage badRequest "Invalid Identifier or URL"
        apiCreateNamed :: ServerPart Response
        apiCreateNamed = do
          method POST
          requireUser auth CreateNamedShortLinks $ \user -> urlIdBodyEndpoint $ \id body -> do
            let mayCreate = validShortLink id || (hasPermissionLevel user ManageShortLinks && validShortLinkWildcard id)
              in if id /= "_" && mayCreate && validRedirectUrl (URL body)
                then liftSIO (putNewShortLink db id $ URL body) $ \success -> if success
                  then ok $ toResponse $ resolveOnBaseUrl baseURL $ T.pack id
                  else errorPage conflict "Conflict"
                else errorPage badRequest "Invalid Identifier or URL"
        apiList :: ServerPart Response
        apiList = do
          nullDir
          method GET
          require auth ManageShortLinks $ do
            liftSIO (M.mapWithKey (\linkId (ShortLink url random) -> ShortLinkEntry url (resolveOnBaseUrl baseURL $ T.pack linkId) random) <$> listShortLinks db) $ \linkMap -> ok $ toResponse $ Aeson.toJSON linkMap
        apiRevise :: ServerPart Response
        apiRevise = do
          method POST
          require auth ManageShortLinks $ urlIdBodyEndpoint $ \id body -> do
            if id /= "_" && validShortLinkWildcard id && validRedirectUrl (URL body)
              then liftSIO (putShortLink db id $ URL body) $ \_ -> ok $ toResponse $ resolveOnBaseUrl baseURL $ T.pack id
              else errorPage badRequest "Invalid Identifier or URL"
        apiDelete :: ServerPart Response
        apiDelete = do
          method POST
          require auth ManageShortLinks $ urlIdEndpoint $ \id -> do
            liftSIO (deleteShortLink db id) $ \_ -> ok $ toResponse $ resolveOnBaseUrl baseURL $ T.pack id

getMe' :: StorageAdapter db => AuthBackend db -> UserPrincipal -> MeResponse
getMe' auth user = MeResponse {
  user = userId user,
  role = userPermissionLevel user,
  auth_methods = authListMethods auth
}

getMe :: StorageAdapter db => AuthBackend db -> UserPrincipal -> ServerPart Response
getMe auth user = ok $ toResponse $ Aeson.toJSON $ getMe' auth user

getMeToken :: StorageAdapter db => AuthBackend db -> UserPrincipal -> ServerPart Response
getMeToken auth user = case userAccessToken user of
  Just accessToken -> ok $ toResponse $ Aeson.toJSON $ TokenResponse { me = getMe' auth user, token = accessToken }
  Nothing -> liftIO (putStrLn "Authentication backend did not create a token.") >> errorPage internalServerError "Internal Server Error"

data MeResponse = MeResponse {
  user :: Maybe String,
  role :: PermissionLevel,
  auth_methods :: [String]
} deriving Generic
instance Aeson.ToJSON MeResponse where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data TokenResponse = TokenResponse {
  token :: String,
  me :: MeResponse
} deriving Generic
instance Aeson.ToJSON TokenResponse where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data ShortLinkEntry = ShortLinkEntry {
  target :: URL,
  link :: URL,
  random :: Bool
} deriving Generic
instance Aeson.ToJSON ShortLinkEntry where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
