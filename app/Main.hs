module Main where

import Cranberry.Config
import Cranberry.Webserver (launchServer)
import Cranberry.Database (connectStorage)
import Cranberry.Auth (connectAuth)

main :: IO ()
main = do
  _ <- putStrLn "Loading configuration."
  config <- loadAppConfig "config.yaml"
  _ <- putStrLn "Connecting to the database."
  db <- connectStorage $ database config
  auth <- connectAuth (ldap config) db
  _ <- putStrLn "Launching the server."
  launchServer (server config) db auth
