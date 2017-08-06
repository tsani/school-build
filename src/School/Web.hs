{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
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
    { unRepoBuildAction :: IO ()
    }

-- | Computes the path to the config directory, creating the directory if it
-- does not exist.
makeConfigPath :: IO FilePath
makeConfigPath = do
  p <- getXdgDirectory XdgConfig "school-build"
  createDirectoryIfMissing True p
  pure p

-- | Start the webservice.
webMain :: RepoKey -> SchoolApiKey -> RepoBuildAction -> IO ()
webMain (RepoKey key) manualKey build = do
  putStrLn "listening on port 8081"
  run 8081 (app manualKey build (gitHubKey (pure key)))

app :: SchoolApiKey -> RepoBuildAction -> GitHubKey -> Application
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
          else liftIO (putStrLn "auth failed") *> throwError err404
      | otherwise -> do
        liftIO $ putStrLn "auth decode failed"
        throwError err404
    Nothing -> do
      liftIO $ putStrLn "no auth header"
      throwError err404

api :: Proxy SchoolApi
api = Proxy

type SchoolApiKey = Text

type SchoolApi
  =
    "github" :> Handle '()
  :<|>
    "manual" :> (
      AuthProtect SchoolApiAuth
        :> PostAccepted '[JSON] ()
    )

type Handle key
  = GitHubEvent '[ 'WebhookPushEvent ]
    :> GitHubSignedReqBody' key '[JSON] Object
    :> PostAccepted '[JSON] ()
  :<|>
    GitHubEvent '[ 'WebhookWildcardEvent ]
    :> GitHubSignedReqBody' key '[JSON] Object
    :> Post '[JSON] ()

data SchoolApiAuth
type instance AuthServerData (AuthProtect SchoolApiAuth) = ()

server :: RepoBuildAction -> Server SchoolApi
server build = (automatic :<|> trivial) :<|> manual where
  manual _ = work
  automatic _ _ = work
  work = liftIO (unRepoBuildAction build)

  trivial = const (const (liftIO $ putStrLn "Got some other event."))
