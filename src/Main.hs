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
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )
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
  (key, deviceId) <- loadPushbulletSettings
  schoolKey <- loadSchoolApiKey
  chan <- startBuilder key deviceId
  webMain schoolKey $ RepoBuildAction (writeChan chan)

loadSchoolApiKey :: IO SchoolApiKey
loadSchoolApiKey = getEnvText "SCHOOL_BUILD_KEY" <|> die "no SCHOOL_BUILD_KEY"

loadPushbulletSettings :: IO (PushbulletKey, DeviceId)
loadPushbulletSettings = pure (,)
  <*> (
    PushbulletKey <$> (
      getEnvText "PUSHBULLET_KEY" <|> die "no PUSHBULLET_KEY"
    )
  )
  <*> (
    DeviceId <$> (
      getEnvText "PUSHBULLET_DEVICE" <|> die "no PUSHBULLET_DEVICE"
    )
  )

-- | Prints a message and exits with a failure status.
die :: String -> IO a
die s = putStrLn s *> exitFailure

getEnvText :: String -> IO (T.Text)
getEnvText = fmap T.pack . getEnv

getEnv' :: String -> IO (Maybe T.Text)
getEnv' s = Just <$> getEnvText s <|> pure Nothing

startBuilder :: PushbulletKey -> DeviceId -> IO (Chan Repo)
startBuilder k d = do
  chan <- newChan
  void . forkIO $ builder k d chan
  pure chan

builder :: PushbulletKey -> DeviceId -> Chan Repo -> IO ()
builder key device chan = do
  manager <- newManager tlsManagerSettings
  let auth = pushbulletAuth key
  let url = BaseUrl Https "api.pushbullet.com" 443 ""
  let env = ClientEnv manager url
  let runHttp = flip runClientM env

  putStrLn "async repo builder started"

  forever $ do
    repo <- readChan chan
    withCurrentDirectory (repoRoot (repoSettings repo)) $ do
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
                  { pushTitle = title
                  , pushBody = T.pack msg
                  }
        Right _ -> do
          putStrLn "Build successful!"
          mapM_ putStr log
          runHttp $ do
            createPush auth $
              simpleNewPush
                ToAll
                NotePush
                  { pushTitle = "Build successful!"
                  , pushBody = "lmao"
                  }

      pure ()

showE :: String -> String -> IO ()
showE s e = do
  putStrLn s
  putStrLn $ "\t" ++ e

cdHome :: IO ()
cdHome = setCurrentDirectory =<< getHomeDirectory
