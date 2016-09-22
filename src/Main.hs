module Main where

import School.Build
import School.Repo
import School.Web

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Monad ( forever, mapM_, void )
import System.Directory
  ( getCurrentDirectory
  , getHomeDirectory
  , setCurrentDirectory
  , withCurrentDirectory
  )
import System.FilePath ( (</>) )

main :: IO ()
main = do
  cdHome
  putStrLn =<< getCurrentDirectory
  chan <- newChan
  let strategy = RepoBuildAction $ \repo -> writeChan chan repo
  void . forkIO . forever $ do
    repo <- readChan chan
    withCurrentDirectory (repoRoot (repoSettings repo)) $ do
      (status, log) <- runUpdateRepo (updateRepo *> runMakefile)
      case status of
        Left e -> case e of
          FailedToFetch msg -> showE "Failed to fetch remote:" msg
          FailedToReset msg -> showE "Failed to reset repo: " msg
          FailedToMake msg -> showE "Failed to build repo: " msg
        Right _ -> do
          putStrLn "Build successful!"
          mapM_ putStr log
  webMain strategy

showE :: String -> String -> IO ()
showE s e = do
  putStrLn s
  putStrLn $ "\t" ++ e

cdHome :: IO ()
cdHome = setCurrentDirectory =<< getHomeDirectory
