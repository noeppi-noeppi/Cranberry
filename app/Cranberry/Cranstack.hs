{-# LANGUAGE OverloadedStrings #-}

module Cranberry.Cranstack (
  module Cranberry.Cranstack,
  module Happstack.Server,
  module Control.Applicative,
  module Control.Monad.IO.Class
) where

import Cranberry.Types
import Happstack.Server hiding (port, unauthorized, found, require)
import Control.Applicative (asum)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Control.Monad
import qualified Cranberry.TemplatePages as Template
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson

withUser :: (Authenticator a) => a -> (UserPrincipal -> ServerPart Response) -> ServerPart Response
withUser auth func = do
  login <- credentials <$> getHeaderM "Authorization"
  case login of
    Just cred -> doLogin cred
    Nothing -> errorPage badRequest "Bad Request"
  where credentials :: Maybe BS.ByteString -> Maybe UserCredentials
        credentials (Just authString)
          | "Basic " `BS.isPrefixOf` authString = case Base64.decode $ BS.drop 6 authString of
            Left err -> Nothing
            Right decoded -> parseHeader $ T.unpack $ TE.decodeUtf8Lenient decoded
          | "Bearer " `BS.isPrefixOf` authString = Just $ Token $ dropWhile (==' ') $ T.unpack $ TE.decodeUtf8Lenient $ BS.drop 7 authString
          | otherwise = Nothing
        credentials Nothing = Just Anonymous
        parseHeader :: String -> Maybe UserCredentials
        parseHeader string = if null password then Nothing else Just $ Login username (tail password)
          where username = takeWhile (':' /=) string
                password = dropWhile (':' /=) string
        doLogin :: UserCredentials -> ServerPart Response
        doLogin login = do
          liftSIO (authenticate auth login) $ \result -> case result of
            ServiceUnavailable -> errorPage serviceUnavailable "Service Unavailable"
            InvalidCredentials -> errorPage unauthorized "Unauthorized"
            Success user -> func user

requireUser :: (Authenticator a) => a -> Permission -> (UserPrincipal -> ServerPart Response) -> ServerPart Response
requireUser auth permission serverPart = withUser auth $ \user -> case user of
  _ | hasPermission user permission -> serverPart user
  _ | isNothing (userId user) -> errorPage unauthorized "Unauthorized"
  _ -> errorPage forbidden "Forbidden"

require :: (Authenticator a) => a -> Permission -> ServerPart Response -> ServerPart Response
require auth permission serverPart = requireUser auth permission (\_ -> serverPart)

unauthorized :: a -> ServerPart a
unauthorized a = do
  setResponseCode 401
  xNoPrompt <- getHeaderM "X-No-Authenticate"
  Control.Monad.when (xNoPrompt /= Just "1") $ setHeaderM "WWW-Authenticate" "Basic realm=\"login\""
  return a

found :: URL -> a -> ServerPart a
found (URL url) a = do
  setResponseCode 302
  setHeaderM "Location" $ T.unpack url
  return a

conflict :: a -> ServerPart a
conflict a = do
  setResponseCode 409
  return a

serviceUnavailable :: a -> ServerPart a
serviceUnavailable a = do
  setResponseCode 503
  return a

instance ToMessage a => ToMessage (MessageContent a) where
  toContentType (MessageContent ctype _) = TE.encodeUtf8 $ T.pack ctype
  toMessage (MessageContent _ message) = toMessage message -- Optional operation
  toResponse (MessageContent ctype message) = setHeaderBS "Content-Type" (TE.encodeUtf8 $ T.pack ctype) origResponse
    where origResponse = toResponse message

instance ToMessage URL where
  toContentType _ = "text/uri-list;charset=UTF-8"
  toMessage (URL url)= LBS.fromStrict $ TE.encodeUtf8 url

instance ToMessage Aeson.Value where
  toContentType _ = "application/json"
  toMessage = Aeson.encode

jsonEndpoint :: (Aeson.FromJSON a) => (a -> ServerPart Response) -> ServerPart Response
jsonEndpoint func = do
  nullDir
  method POST
  maybeBody <- fmap unBody <$> (askRq >>= takeRequestBody)
  case maybeBody of
    Just body -> case Aeson.decode body of
      Just parsedBody -> func parsedBody
      Nothing -> errorPage badRequest "Invalid Request Body"
    Nothing -> errorPage badRequest "Missing Request Body"

_urlIdEndpoint :: Method -> (String -> ServerPart Response) -> ServerPart Response
_urlIdEndpoint httpMethod func = path $ \id -> do
  nullDir
  method httpMethod
  func id

urlIdEndpoint :: (String -> ServerPart Response) -> ServerPart Response
urlIdEndpoint = _urlIdEndpoint POST

withBody :: (T.Text -> ServerPart Response) -> ServerPart Response
withBody func = do
  maybeBody <- fmap unBody <$> (askRq >>= takeRequestBody)
  case maybeBody of
    Just body | not $ LBS.null body -> func $ TE.decodeUtf8Lenient $ LBS.toStrict body
    _ -> errorPage badRequest "Missing Request Body"

urlIdBodyEndpoint :: (String -> T.Text -> ServerPart Response) -> ServerPart Response
urlIdBodyEndpoint func = _urlIdEndpoint POST $ \id -> withBody $ \body -> func id body

serve :: (ToMessage a) => a -> ServerPart Response
serve content = do
  nullDir
  method GET
  ok $ toResponse content

errorPage :: (Response -> ServerPart Response) -> String -> ServerPart Response
errorPage factory message = do
  acceptHeader <- getHeaderM "Accept"
  case acceptHeader of
    Just accept | "text/html" `T.isInfixOf` lowercaseByteString accept -> factory $ toResponse $ Template.errorPage message
    _ -> factory $ toResponse message
  where lowercaseByteString :: BS.ByteString -> T.Text
        lowercaseByteString bytes = T.toLower $ TE.decodeUtf8Lenient bytes

liftSIO :: IO a -> (a -> ServerPart Response) -> ServerPart Response
liftSIO io func = do
  result <- liftIO (tryAny io)
  case result of
    Right value -> func value
    Left err -> liftIO (putStrLn "HTTP handler error") >> liftIO (print err) >> errorPage internalServerError "Internal Server Error"
  where tryAny :: IO a -> IO (Either SomeException a)
        tryAny = try
