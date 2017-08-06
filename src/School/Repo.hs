{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module School.Repo where

import Data.ByteString ( ByteString )
import System.FilePath

data Repo
  = Repo
    { repoRoot :: !FilePath
    }

newtype RepoKey = RepoKey ByteString
