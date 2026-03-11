module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Cranberry.Config
import Cranberry.Webserver (launchServer)
import Cranberry.Database (connectDatabase, setupDatabase)
import Cranberry.StoragePool (connectPooledStorage, setupPooledStorage)

main :: IO ()
main = do
  configFilePath <- getArgs >>= \case
    [] -> putStrLn "Missing config file name argument" >> exitWith (ExitFailure 1)
    path : [] -> pure path
    _ -> putStrLn "Invalid usage." >> exitWith (ExitFailure 1)
  putStrLn "Loading configuration."
  config <- loadAppConfig configFilePath
  putStrLn "Connecting to the database."
  db <- connectPooledStorage (connectDatabase $ database config)
  setupPooledStorage setupDatabase db
  putStrLn "Launching the server."
  launchServer (server config) db (auth config)
