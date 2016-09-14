{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module School.Web where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( Object )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
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
    { unRepoBuildAction :: IO ()
    }

-- | Computes the path to the config directory, creating the directory if it
-- does not exist.
makeConfigPath :: IO FilePath
makeConfigPath = do
  p <- getXdgDirectory XdgConfig "school-build"
  createDirectoryIfMissing True p
  pure p

-- | Gets the path to the secret key associated with the school repo.
getKeyPath :: IO FilePath
getKeyPath = (</> "key.bin") <$> makeConfigPath

-- | Start the webservice.
webMain :: RepoBuildAction -> IO ()
webMain build = do
  [key] <- map (gitHubKey . pure) . C8.lines <$> (BS.readFile =<< getKeyPath)
  run 8081 (app build key)

app :: RepoBuildAction -> GitHubKey -> Application
app build key = serveWithContext api (key :. EmptyContext) (server build)

api :: Proxy SchoolApi
api = Proxy

type SchoolApi
  = "github" :> (
      GitHubEvent '[ 'WebhookPushEvent ]
    :> GitHubSignedReqBody '[JSON] Object
    :> PostAccepted '[JSON] ()
  :<|>
      GitHubEvent '[ 'WebhookWildcardEvent ]
    :> GitHubSignedReqBody '[JSON] Object
    :> Post '[JSON] ()
  )

server :: RepoBuildAction -> Server SchoolApi
server build
  = buildHandler build
  :<|> const (const (liftIO $ putStrLn "Got some other event."))

buildHandler :: RepoBuildAction -> RepoWebhookEvent -> Object -> Handler ()
buildHandler build _ _ = beginBuild where
  beginBuild = liftIO (unRepoBuildAction build)
