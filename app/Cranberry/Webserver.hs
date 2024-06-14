module Cranberry.Webserver where

import Cranberry.Types
import Cranberry.Cranstack
import qualified Cranberry.TemplatePages as Template
import qualified Happstack.Server
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

data WebserverConfig = WebserverConfig {
  self :: String,
  port :: Int
} deriving (Show, Generic)

instance Configuration WebserverConfig where
  defaultConfiguration = WebserverConfig {
    self = "http://localhost/",
    port = 80
  }

launchServer :: (StorageAdapter s, Authenticator a) => WebserverConfig -> s -> a -> IO ()
launchServer config db auth = simpleHTTP serverConfig (route db auth $ URL $ T.pack $ self config)
  where serverConfig :: Conf
        serverConfig = Conf {
          Happstack.Server.port = port config,
          validator = validator nullConf,
          logAccess = logAccess nullConf,
          timeout = timeout nullConf,
          threadGroup = threadGroup nullConf
        }

route :: (StorageAdapter s, Authenticator a) => s -> a -> URL -> ServerPart Response
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
        redirect = path $ \id -> do
          nullDir
          method GET
          guardRq (\_ -> validShortLink id)
          liftSIO (getShortLink db id) $ \dest -> case dest of
            Just url -> found url $ toResponse "Found"
            Nothing -> errorPage notFound "Not Found"
        redirectRecursive = path $ \id -> do
          method GET
          guardRq (\_ -> validShortLink id)
          liftSIO (getShortLink db $ id ++ "*") $ \dest -> case dest of
            Just url -> uriRest $ \rest -> found (resolveOnBaseUrl url (T.pack rest)) $ toResponse "Found"
            Nothing -> errorPage notFound "Not Found"
        control :: ServerPart Response
        control = asum [
          dir "index.js" $ serve Template.indexScript,
          dir "style.css" $ serve Template.stylesheet,
          dir "api" api,
          serve Template.indexPage,
          errorPage notFound "Not found"]
        api :: ServerPart Response
        api = asum [
          dir "me" apiMe,
          dir "grant" apiGrant,
          dir "revoke" apiRevoke,
          dir "create" apiCreate,
          dir "list" apiList,
          dir "revise" apiRevise,
          dir "delete" apiDelete,
          errorPage notFound "Not found"]
        apiMe :: ServerPart Response
        apiMe = withUser auth $ \user -> do
          nullDir
          method GET
          ok $ toResponse $ Aeson.toJSON $ getMe user
        apiGrant :: ServerPart Response
        apiGrant = do
          nullDir
          method POST
          withUser auth $ \user -> case (userId user, userAccessToken user) of
            (Just username, Nothing) -> liftSIO (createAccessToken db username) $ \token -> ok $ toResponse $ Aeson.toJSON TokenResponse {
              me = getMe user,
              token = token
            }
            _ -> errorPage forbidden "Forbidden"
        apiRevoke :: ServerPart Response
        apiRevoke = do
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
            liftSIO (putRandomShortLink db $ URL body) $ \id -> ok $ toResponse $ resolveOnBaseUrl baseURL $ T.pack id
        apiCreateNamed :: ServerPart Response
        apiCreateNamed = do
          method POST
          requireUser auth CreateNamedShortLinks $ \user -> urlIdBodyEndpoint $ \id body -> do
            let mayCreate = validShortLink id || (hasPermission user ManageShortLinks && validShortLinkWildcard id)
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
            liftSIO (listShortLinks db) $ \linkMap -> ok $ toResponse $ Aeson.toJSON linkMap
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

getMe :: UserPrincipal -> MeResponse
getMe user = MeResponse {
  user = userId user,
  role = case True of
    _ | hasPermission user ManageShortLinks -> "manage"
    _ | hasPermission user CreateNamedShortLinks -> "create_named"
    _ | hasPermission user CreateAnonymousShortLinks -> "create_anonymous"
    _ -> "none"
  }

data MeResponse = MeResponse {
  user :: Maybe String,
  role :: String
} deriving Generic
instance Aeson.ToJSON MeResponse where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data TokenResponse = TokenResponse {
  token :: String,
  me :: MeResponse
} deriving Generic
instance Aeson.ToJSON TokenResponse where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
