module Cranberry.Database (PostgresConfig (..), StorageAdapter, connectDatabase, setupDatabase) where

import Cranberry.Types
import Opaleye
import Opaleye.SqlTypes
import Opaleye.Internal.Table (tableIdentifier)
import Opaleye.Internal.PrimQuery (tiSchemaName, tiTableName)
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Profunctor.Product as P
import qualified Data.Time as Time
import qualified System.Random as Rand

data PostgresConfig = PostgresConfig {
  host :: String,
  port :: Int,
  database :: Maybe String,
  user :: String,
  password :: Maybe String,
  options :: String
} deriving (Show, Generic)

instance Configuration PostgresConfig where
  defaultConfiguration = PostgresConfig {
    host = "localhost",
    port = 5432,
    user = "postgres",
    password = Nothing,
    database = Nothing,
    options = ""
  }

instance Disposable PG.Connection where
  dispose = PG.close

connectDatabase :: PostgresConfig -> IO PG.Connection
connectDatabase config = do
  connectInfo <- return $ PG.ConnectInfo {
    PG.connectHost = host config,
    PG.connectPort = fromIntegral $ port config,
    PG.connectUser = user config,
    PG.connectPassword = maybe "" id $ password config,
    PG.connectDatabase = maybe (user config) id $ database config
  }
  let connStr = BS.concat [PG.postgreSQLConnectionString connectInfo, " ", (T.encodeUtf8 $ T.pack $ options config)]
  con <- PG.connectPostgreSQL connStr
  return con

setupDatabase :: PG.Connection -> IO ()
setupDatabase con = PG.withTransaction con $ do
  _ <- PG.execute con (fromString "CREATE TABLE IF NOT EXISTS short_links (\
    \name             TEXT PRIMARY KEY NOT NULL,\
    \destination      TEXT NOT NULL,\
    \random           BOOLEAN NOT NULL);") ()
  _ <- PG.execute con (fromString "CREATE TABLE IF NOT EXISTS access_tokens (\
    \token            TEXT PRIMARY KEY NOT NULL,\
    \username         TEXT NOT NULL,\
    \permission_level INTEGER NOT NULL,\
    \expires          TIMESTAMP WITH TIME ZONE NOT NULL);") ()
  return ()

instance StorageAdapter PG.Connection where
  getShortLink con id = do
    rs <- runSelect con $ shortLinkSelect id :: IO [T.Text]
    return $ URL <$> listToMaybe rs
  putShortLink con id dest = PG.withTransaction con $ do
    _ <- runUpdate con $ shortLinkUpdate id dest
    _ <- runInsert con $ shortLinkInsert id False dest
    return ()
  putNewShortLink con id dest = doPutNewShortLink con id False dest
  putRandomShortLink con url = do
    id <- randomId
    success <- doPutNewShortLink con id True url
    if success
      then return id
      else putRandomShortLink con url
  deleteShortLink con id = do
    _ <- runDelete con $ shortLinkDelete id
    return ()
  listShortLinks con = do
    rs <- runSelect con shortLinkListSelect :: IO [(String, T.Text, Bool)]
    return $ Map.fromList (map (\(name, url, random) -> (name, ShortLink (URL url) random)) rs)
  getAccessTokenDetails con token = do
    rs <- runSelect con $ accessTokenSelect token :: IO [(String, Int)]
    return $ listToMaybe [(username, permissionLevelFromCode permissionCode) | (username, permissionCode) <- rs]
  createAccessToken con user permissionLevel = do
    _ <- runDelete con accessTokenClean
    token <- randomAccessToken
    success <- putNewAccessToken token user permissionLevel
    if success
      then return token
      else createAccessToken con user permissionLevel
    where putNewAccessToken :: String -> String -> PermissionLevel -> IO Bool
          putNewAccessToken token user permissionLevel = do
            modifiedCount <- runInsert con $ accessTokenInsert token user permissionLevel (Time.calendarTimeTime 1200)
            return $ modifiedCount > 0
  revokeAccessToken con token = do
    _ <- runDelete con accessTokenClean
    _ <- runDelete con $ accessTokenDelete token
    return ()

doPutNewShortLink :: PG.Connection -> String -> Bool -> URL -> IO Bool
doPutNewShortLink con id random dest = do
    modifiedCount <- runInsert con $ shortLinkInsert id random dest
    return $ modifiedCount > 0

type SymmetricTable a = Table a a

tableShortLinks :: SymmetricTable (Field SqlText, Field SqlText, Field SqlBool)
tableShortLinks = table "short_links" $ P.p3 (
  tableField "name",
  tableField "destination",
  tableField "random")

tableAccessTokens :: SymmetricTable (Field SqlText, Field SqlText, Field SqlInt4, Field SqlTimestamptz)
tableAccessTokens = table "access_tokens" $ P.p4 (
  tableField "token",
  tableField "username",
  tableField "permission_level",
  tableField "expires")

shortLinkListSelect :: Select (Field SqlText, Field SqlText, Field SqlBool)
shortLinkListSelect = selectTable tableShortLinks

shortLinkSelect :: String -> Select (Field SqlText)
shortLinkSelect linkId = do
  (name, dest, _) <- selectTable tableShortLinks
  where_ (name .== sqlString linkId)
  return dest

shortLinkInsert :: String -> Bool -> URL -> Insert Int64
shortLinkInsert linkId random (URL dest) = Insert {
  iTable = tableShortLinks,
  iRows = [(sqlString linkId, sqlStrictText dest, sqlBool random)],
  iReturning = rCount,
  iOnConflict = Just doNothing
}

shortLinkUpdate :: String -> URL -> Update Int64
shortLinkUpdate linkId (URL dest) = Update {
  uTable = tableShortLinks,
  uUpdateWith = updateEasy $ \(_, _, random) -> (sqlString linkId, sqlStrictText dest, random),
  uWhere = \(name, _, _) -> name .== sqlString linkId,
  uReturning = rCount
}

shortLinkDelete :: String -> Delete Int64
shortLinkDelete id = Delete {
  dTable = tableShortLinks,
  dWhere = \(name, _, _) -> name .== sqlString id,
  dReturning = rCount
}

accessTokenSelect :: String -> Select (Field SqlText, Field SqlInt4)
accessTokenSelect requestedToken = do
  (token, user, permissionLevel, expires) <- selectTable tableAccessTokens
  where_ (expires .>= now)
  where_ (token   .== sqlString requestedToken)
  return (user, permissionLevel)

accessTokenClean :: Delete Int64
accessTokenClean = Delete {
  dTable = tableAccessTokens,
  dWhere = \(_, _, _, expires) -> expires .< now,
  dReturning = rCount
}

accessTokenInsert :: String -> String -> PermissionLevel -> Time.CalendarDiffTime -> Insert Int64
accessTokenInsert token user permissionLevel validFor = Insert {
  iTable = tableAccessTokens,
  iRows = [(sqlString token, sqlString user, sqlInt4 (permissionCodeFromLevel permissionLevel), addInterval now (sqlInterval validFor))],
  iReturning = rCount,
  iOnConflict = Just doNothing
}

accessTokenDelete :: String -> Delete Int64
accessTokenDelete token = Delete {
  dTable = tableAccessTokens,
  dWhere = \(tk, _, _, _) -> tk .== sqlString token,
  dReturning = rCount
}

randomId :: IO String
randomId = randomSequence 7

randomAccessToken :: IO String
randomAccessToken = randomSequence 42

randomSequence :: Int ->  IO String
randomSequence len = sequence $ replicate len randomChar
  where randomChar :: IO Char
        randomChar = charMap <$> Rand.getStdRandom (Rand.uniformR (0, 61))
        charMap :: Int -> Char
        charMap i
          | i < 26 = Data.Char.chr $ 97 + i
          | i < 52 = Data.Char.chr $ 39 + i
          | i < 62 = Data.Char.chr $ i - 4

permissionCodeFromLevel :: PermissionLevel -> Int
permissionCodeFromLevel NoPermission = 0
permissionCodeFromLevel CreateAnonymousShortLinks = 1
permissionCodeFromLevel CreateNamedShortLinks = 2
permissionCodeFromLevel ManageShortLinks = 3

permissionLevelFromCode :: Int -> PermissionLevel
permissionLevelFromCode 1 = CreateAnonymousShortLinks
permissionLevelFromCode 2 = CreateNamedShortLinks
permissionLevelFromCode 3 = ManageShortLinks
permissionLevelFromCode _ = NoPermission
