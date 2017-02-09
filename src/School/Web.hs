{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module School.Web where

import School.Repo

import Control.Monad ( forM )
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( Object, FromJSON, ToJSON, encode, decode )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8' )
import Data.Maybe ( fromJust )
import Network.HTTP.Types.Header ( hAuthorization )
import Network.Wai ( Application, Request, requestHeaders )
import Network.Wai.Handler.Warp ( run )
import Servant
import Servant.Server.Experimental.Auth
import Servant.API.Verbs ( PostAccepted )
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
webMain :: SchoolApiKey -> RepoBuildAction -> IO ()
webMain manualKey build = do
  key <- loadGitHubKey
  putStrLn "listening on port 8081"
  run 8081 (app manualKey build key)

app :: SchoolApiKey -> RepoBuildAction -> MyGitHubKey -> Application
app manualKey build ghKey = serveWithContext api ctx (server build) where
  ctx = checkManualAuth manualKey :. ghKey :. EmptyContext

checkManualAuth :: SchoolApiKey -> AuthHandler Request ()
checkManualAuth key = AuthHandler $ \r -> do
  let hds = requestHeaders r
  case lookup hAuthorization hds of
    Just (decodeUtf8' -> givenKeyE)
      | Right givenKey <- givenKeyE -> do
        if givenKey == key
          then liftIO $ putStrLn "passed auth"
          else throwError err404
      | otherwise -> throwError err404
    Nothing -> throwError err404

api :: Proxy SchoolApi
api = Proxy

type SchoolApiKey = Text
type MyGitHubKey = GitHubKey' Repo

type SchoolApi
  =
    "github" :> (
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
  :<|>
    "manual" :> (
      AuthProtect SchoolApiAuth
        :> Capture "repo" Repo
        :> PostAccepted '[JSON] ()
    )

data SchoolApiAuth
type instance AuthServerData (AuthProtect SchoolApiAuth) = ()

server :: RepoBuildAction -> Server SchoolApi
server build = (schoolRepo :<|> cvRepo) :<|> manual where
  schoolRepo = buildHandler build :<|> trivial
  cvRepo = buildHandler build :<|> trivial
  trivial = const (const (liftIO $ putStrLn "Got some other event."))
  manual () repo
    = buildHandler
      build
      (error "accessed webhookevent for manual build")
      (repo, error "accessed request body (doesn't exist) for manual build")

buildHandler
  :: RepoBuildAction
  -> RepoWebhookEvent
  -> (Repo, Object)
  -> Handler ()
buildHandler build _ (repo, _) = beginBuild where
  beginBuild = liftIO (unRepoBuildAction build repo)
