{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module School.Repo where

import Servant.GitHub.Webhook

import System.FilePath

data RepoSettings
  = RepoSettings
    { repoRoot :: !FilePath
    }

data Repo
  = SchoolRepo
  | CvRepo
  deriving (Bounded, Enum, Eq, Show)

repos :: [Repo]
repos = [minBound .. maxBound]

repoName :: Repo -> String
repoName = \case
  SchoolRepo -> "school"
  CvRepo -> "cv"

keyFileName :: Repo -> FilePath
keyFileName = (++ "-key.bin") . repoName

repoSettings :: Repo -> RepoSettings
repoSettings = \case
  SchoolRepo -> RepoSettings { repoRoot = "school" }
  CvRepo -> RepoSettings { repoRoot = "projects" </> "cv" }

type instance Demote' ('KProxy :: KProxy Repo) = Repo
instance Reflect 'SchoolRepo where
  reflect _ = SchoolRepo
instance Reflect 'CvRepo where
  reflect _ = CvRepo
