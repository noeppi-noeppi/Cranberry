module Main where

import Cranberry.Config
import Cranberry.Webserver (launchServer)
import Cranberry.Database (connectDatabase, setupDatabase)
import Cranberry.StoragePool (connectPooledStorage, setupPooledStorage)
import Cranberry.Auth (connectAuth)

main :: IO ()
main = do
  _ <- putStrLn "Loading configuration."
  config <- loadAppConfig "config.yaml"
  _ <- putStrLn "Connecting to the database."
  db <- connectPooledStorage (connectDatabase $ database config)
  _ <- setupPooledStorage setupDatabase db
  auth <- connectAuth (ldap config) db
  _ <- putStrLn "Launching the server."
  launchServer (server config) db auth
