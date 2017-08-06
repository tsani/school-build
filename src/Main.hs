{-# LANGUAGE OverloadedStrings #-}

module Main where

import School.Build
import School.Repo
import School.Web

import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Types

import Control.Applicative ( (<|>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Monad ( forever, mapM_, void )
import qualified Data.ByteString as BS
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Servant.Client
import System.Directory
  ( getCurrentDirectory
  , getHomeDirectory
  , setCurrentDirectory
  , withCurrentDirectory
  )
import System.Environment ( getEnv )
import System.Exit ( exitFailure )
import System.FilePath ( (</>) )

main :: IO ()
main = do
  cdHome
  putStrLn =<< getCurrentDirectory
  key <- loadPushbulletKey
  schoolKey <- loadSchoolApiKey
  repoKey <- loadRepoKey
  repoRoot <- Repo <$> loadRepoRoot
  chan <- startBuilder key
  webMain repoKey schoolKey $ RepoBuildAction (writeChan chan repoRoot)

loadSchoolApiKey :: IO SchoolApiKey
loadSchoolApiKey = getEnvText "SCHOOL_BUILD_KEY" <|> die "no SCHOOL_BUILD_KEY"

loadRepoRoot :: IO FilePath
loadRepoRoot
  = getEnv "SCHOOL_BUILD_REPO_ROOT" <|> die "no SCHOOL_BUILD_REPO_ROOT"

loadRepoKey :: IO RepoKey
loadRepoKey =
  RepoKey . encodeUtf8 <$> getEnvText "SCHOOL_BUILD_REPO_KEY"
  <|> die "no SCHOOL_BUILD_REPO_KEY"

loadPushbulletKey :: IO PushbulletKey
loadPushbulletKey =
  PushbulletKey <$> (
    getEnvText "PUSHBULLET_KEY" <|> die "no PUSHBULLET_KEY"
  )

-- | Prints a message and exits with a failure status.
die :: String -> IO a
die s = putStrLn s *> exitFailure

getEnvText :: String -> IO (T.Text)
getEnvText = fmap T.pack . getEnv

getEnv' :: String -> IO (Maybe T.Text)
getEnv' s = Just <$> getEnvText s <|> pure Nothing

startBuilder :: PushbulletKey -> IO (Chan Repo)
startBuilder k = do
  chan <- newChan
  void . forkIO $ builder k chan
  pure chan

builder :: PushbulletKey -> Chan Repo -> IO ()
builder key chan = do
  manager <- newManager tlsManagerSettings
  let auth = pushbulletAuth key
  let url = BaseUrl Https "api.pushbullet.com" 443 ""
  let env = ClientEnv manager url
  let runHttp = flip runClientM env

  putStrLn "async repo builder started"

  forever $ do
    (Repo root) <- readChan chan
    withCurrentDirectory root $ do
      (status, log) <- runUpdateRepo (updateRepo *> runMakefile)
      _ <- case status of
        Left e -> do
          (title, msg) <- pure $ case e of
            FailedToFetch msg -> ("could not fetch remote", msg)
            FailedToReset msg -> ("could not reset repo", msg)
            FailedToMake msg -> ("could not build repo", msg)

          runHttp $ do
            createPush auth $
              simpleNewPush
                ToAll
                NotePush
                  { pushTitle = pure "Build completed"
                  , pushBody = "Failed (" <> title <> ")\n" <> T.pack msg
                  }
        Right _ -> do
          putStrLn "Build successful!"
          mapM_ putStr log
          runHttp $ do
            createPush auth $
              simpleNewPush
                ToAll
                NotePush
                  { pushTitle = pure "Build completed"
                  , pushBody = "Build successfully completed."
                  }

      pure ()

showE :: String -> String -> IO ()
showE s e = do
  putStrLn s
  putStrLn $ "\t" ++ e

cdHome :: IO ()
cdHome = setCurrentDirectory =<< getHomeDirectory
