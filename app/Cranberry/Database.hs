module Cranberry.Database where

import Cranberry.Types
import Opaleye
import Opaleye.SqlTypes
import Opaleye.Internal.Table (tableIdentifier)
import Opaleye.Internal.PrimQuery (tiSchemaName, tiTableName)
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.Char
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Profunctor.Product as P
import qualified Data.Time as Time
import qualified System.Random as Rand

data PostgresConfig = PostgresConfig {
  host :: String,
  port :: Int,
  database :: Maybe String,
  user :: String,
  password :: Maybe String
} deriving (Show, Generic)

instance Configuration PostgresConfig where
  defaultConfiguration = PostgresConfig {
    host = "localhost",
    port = 5432,
    user = "postgres",
    password = Nothing,
    database = Nothing
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
  con <- PG.connect connectInfo
  return con

setupDatabase :: PG.Connection -> IO ()
setupDatabase con = PG.withTransaction con $ do
  _ <- PG.execute con (fromString "CREATE TABLE IF NOT EXISTS short_links (\
    \name        TEXT PRIMARY KEY NOT NULL,\
    \destination TEXT NOT NULL);") ()
  _ <- PG.execute con (fromString "CREATE TABLE IF NOT EXISTS access_tokens (\
    \token       TEXT PRIMARY KEY NOT NULL,\
    \username    TEXT NOT NULL,\
    \expires     TIMESTAMP WITH TIME ZONE NOT NULL);") ()
  return ()

instance StorageAdapter PG.Connection where
  getShortLink con id = do
    rs <- runSelect con $ shortLinkSelect id :: IO [T.Text]
    return $ URL <$> listToMaybe rs
  putShortLink con id dest = PG.withTransaction con $ do
    _ <- runUpdate con $ shortLinkUpdate id dest
    _ <- runInsert con $ shortLinkInsert id dest
    return ()
  putNewShortLink con id dest = do
    modifiedCount <- runInsert con $ shortLinkInsert id dest
    return $ modifiedCount > 0
  putRandomShortLink con url = do
    id <- randomId
    success <- putNewShortLink con id url
    if success
      then return id
      else putRandomShortLink con url
  deleteShortLink con id = do
    _ <- runDelete con $ shortLinkDelete id
    return ()
  listShortLinks con = do
    rs <- runSelect con shortLinkListSelect :: IO [(String, T.Text)]
    return $ Map.map URL $ Map.fromList rs
  getUserForAccessToken con token = do
    rs <- runSelect con $ accessTokenSelect token :: IO [String]
    return $ listToMaybe rs
  createAccessToken con user = do
    _ <- runDelete con accessTokenClean
    token <- randomAccessToken
    success <- putNewAccessToken token user
    if success
      then return token
      else createAccessToken con user
    where putNewAccessToken :: String -> String -> IO Bool
          putNewAccessToken token user = do
            modifiedCount <- runInsert con $ accessTokenInsert token user (Time.calendarTimeTime 600)
            return $ modifiedCount > 0
  revokeAccessToken con token = do
    _ <- runDelete con accessTokenClean
    _ <- runDelete con $ accessTokenDelete token
    return ()



type SymmetricTable a = Table a a

tableShortLinks :: SymmetricTable (Field SqlText, Field SqlText)
tableShortLinks = table "short_links" $ P.p2 (
  tableField "name",
  tableField "destination")

tableAccessTokens :: SymmetricTable (Field SqlText, Field SqlText, Field SqlTimestamptz)
tableAccessTokens = table "access_tokens" $ P.p3 (
  tableField "token",
  tableField "username",
  tableField "expires")

shortLinkListSelect :: Select (Field SqlText, Field SqlText)
shortLinkListSelect = selectTable tableShortLinks

shortLinkSelect :: String -> Select (Field SqlText)
shortLinkSelect id = do
  (name, dest) <- selectTable tableShortLinks
  where_ (name .== sqlString id)
  return dest

shortLinkInsert :: String -> URL -> Insert Int64
shortLinkInsert id (URL dest) = Insert {
  iTable = tableShortLinks,
  iRows = [(sqlString id, sqlStrictText dest)],
  iReturning = rCount,
  iOnConflict = Just doNothing
}

shortLinkUpdate :: String -> URL -> Update Int64
shortLinkUpdate id (URL dest) = Update {
  uTable = tableShortLinks,
  uUpdateWith = updateEasy $ \(_, _) -> (sqlString id, sqlStrictText dest),
  uWhere = \(name, _) -> name .== sqlString id,
  uReturning = rCount
}

shortLinkDelete :: String -> Delete Int64
shortLinkDelete id = Delete {
  dTable = tableShortLinks,
  dWhere = \(name, _) -> name .== sqlString id,
  dReturning = rCount
}

accessTokenSelect :: String -> Select (Field SqlText)
accessTokenSelect token = do
  (token, user, expires) <- selectTable tableAccessTokens
  where_ (expires  .>= now)
  return user

accessTokenClean :: Delete Int64
accessTokenClean = Delete {
  dTable = tableAccessTokens,
  dWhere = \(_, _, expires) -> expires .< now,
  dReturning = rCount
}

accessTokenInsert :: String -> String -> Time.CalendarDiffTime -> Insert Int64
accessTokenInsert token user validFor = Insert {
  iTable = tableAccessTokens,
  iRows = [(sqlString token, sqlString user, addInterval now (sqlInterval validFor))],
  iReturning = rCount,
  iOnConflict = Just doNothing
}

accessTokenDelete :: String -> Delete Int64
accessTokenDelete token = Delete {
  dTable = tableAccessTokens,
  dWhere = \(tk, _, _) -> tk .== sqlString token,
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
