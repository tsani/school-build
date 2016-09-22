{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module School.Web where

import School.Repo

import Control.Monad ( forM )
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( Object, FromJSON, ToJSON, encode, decode )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Maybe ( fromJust )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant
import Servant.GitHub.Webhook
import System.FilePath ( (</>) )
import System.Directory
  ( createDirectoryIfMissing
  , getXdgDirectory
  , XdgDirectory(XdgConfig)
  )

-- | A wrapper for a strategy to compile the repository.
--
-- The strategy should execute the build asynchronously, as to not block the
-- webservice.
newtype RepoBuildAction
  = RepoBuildAction
    { unRepoBuildAction :: Repo -> IO ()
    }

-- | Computes the path to the config directory, creating the directory if it
-- does not exist.
makeConfigPath :: IO FilePath
makeConfigPath = do
  p <- getXdgDirectory XdgConfig "school-build"
  createDirectoryIfMissing True p
  pure p

-- | Gets the path to the secret key associated with the school repo.
getKeyPath :: Repo -> IO FilePath
getKeyPath r = (</> repoName r ++ "-key.bin") <$> makeConfigPath

loadGitHubKey :: IO MyGitHubKey
loadGitHubKey = do
  confDir <- makeConfigPath
  assoc <- forM repos $ \repo -> (,) <$> pure repo
    <*> (head . C8.lines <$> BS.readFile (confDir </> keyFileName repo))
  pure $ GitHubKey (pure . fromJust . flip lookup assoc)

-- | Start the webservice.
webMain :: RepoBuildAction -> IO ()
webMain build = do
  key <- loadGitHubKey
  run 8081 (app build key)

app :: RepoBuildAction -> MyGitHubKey -> Application
app build key = serveWithContext api (key :. EmptyContext) (server build)

api :: Proxy SchoolApi
api = Proxy

type MyGitHubKey = GitHubKey' Repo

type SchoolApi
  = "github" :> (
    "school" :> (
      GitHubEvent '[ 'WebhookPushEvent ]
      :> GitHubSignedReqBody' 'SchoolRepo '[JSON] Object
      :> PostAccepted '[JSON] ()
    :<|>
      GitHubEvent '[ 'WebhookWildcardEvent ]
      :> GitHubSignedReqBody' 'SchoolRepo '[JSON] Object
      :> Post '[JSON] ()
    )
  :<|>
    "cv" :> (
      GitHubEvent '[ 'WebhookPushEvent ]
      :> GitHubSignedReqBody' 'CvRepo '[JSON] Object
      :> PostAccepted '[JSON] ()
    :<|>
      GitHubEvent '[ 'WebhookWildcardEvent ]
      :> GitHubSignedReqBody' 'CvRepo '[JSON] Object
      :> Post '[JSON] ()
    )
  )

server :: RepoBuildAction -> Server SchoolApi
server build = schoolRepo :<|> cvRepo where
  schoolRepo = buildHandler build :<|> trivial
  cvRepo = buildHandler build :<|> trivial
  trivial = const (const (liftIO $ putStrLn "Got some other event."))

buildHandler
  :: RepoBuildAction
  -> RepoWebhookEvent
  -> (Repo, Object)
  -> Handler ()
buildHandler build _ (repo, _) = beginBuild where
  beginBuild = liftIO (unRepoBuildAction build repo)
