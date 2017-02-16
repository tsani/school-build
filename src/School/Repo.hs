{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module School.Repo where

import Control.Applicative ( Alternative(..) )
import Data.Monoid ( mempty,(<>) )
import Data.String ( IsString )
import Servant.GitHub.Webhook
import Web.HttpApiData ( FromHttpApiData(..) )

import System.FilePath

data RepoSettings
  = RepoSettings
    { repoRoot :: !FilePath
    }

data Repo
  = SchoolRepo
  | CvRepo
  | BlogRepo
  deriving (Bounded, Enum, Eq, Show)

-- | The check monad has a goofy alternative instance, in which runs of 'Left'
-- values are collapsed via a monoid instance, but as soon as a 'Right' value
-- is encountered, then all the failures are ignored.
newtype Check e a = Check { check :: Either e a }
  deriving (Functor, Applicative)

instance Monoid m => Alternative (Check m) where
  empty = Check (Left mempty)
  Check (Right x) <|> _ = pure x
  Check (Left _) <|> Check (Right x) = pure x
  Check (Left e1) <|> Check (Left e2) = Check (Left (e1 <> e2))

instance FromHttpApiData Repo where
  parseUrlPiece p
    = check matches where
      matchRepo repo = match repo (repoName repo)
      matches
        = matchRepo CvRepo
        <|> matchRepo SchoolRepo
        <|> matchRepo BlogRepo
      match t s = if s == p then pure t else Check (Left ("not " <> s))

repos :: [Repo]
repos = [minBound .. maxBound]

repoName :: IsString a => Repo -> a
repoName = \case
  SchoolRepo -> "school"
  CvRepo -> "cv"
  BlogRepo -> "blog"

keyFileName :: Repo -> FilePath
keyFileName = (++ "-key.bin") . repoName

repoSettings :: Repo -> RepoSettings
repoSettings = \case
  SchoolRepo -> RepoSettings { repoRoot = "school" }
  CvRepo -> RepoSettings { repoRoot = "projects" </> "cv" }
  BlogRepo -> RepoSettings { repoRoot = "projects" </> "jerrington.me" }

type instance Demote' ('KProxy :: KProxy Repo) = Repo
instance Reflect 'SchoolRepo where
  reflect _ = SchoolRepo
instance Reflect 'CvRepo where
  reflect _ = CvRepo
instance Reflect 'BlogRepo where
  reflect _ = BlogRepo
