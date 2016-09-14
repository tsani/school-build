{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module School.Build where

import Control.Monad.Except
import Control.Monad.Writer
import System.Exit ( ExitCode(..) )
import System.Process ( readProcessWithExitCode )

-- | Errors that can occur during the local repository update.
data UpdateRepoError
  -- | An error occurred while trying to fetch the remote repository.
  = FailedToFetch
    String -- ^ The standard error emitted by git.
  -- | An error occurred while trying to reset the local master branch to that
  -- of the remote's master branch.
  | FailedToReset
    String -- ^ The standard error emitted by git.
  -- | The execution of the root Makefile failed.
  | FailedToMake
    String -- ^ The standard error emitted by make.

-- | Monad transformer for performing a repository update.
newtype UpdateRepoT m a
  = UpdateRepoT
    { unUpdateRepoT :: ExceptT UpdateRepoError (WriterT [String] m) a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError UpdateRepoError
    , MonadWriter [String]
    )

instance MonadTrans UpdateRepoT where
  lift = UpdateRepoT . lift . lift

type UpdateRepo = UpdateRepoT IO

runUpdateRepoT
  :: Monad m
  => UpdateRepoT m a
  -> m (Either UpdateRepoError a, [String])
runUpdateRepoT (UpdateRepoT m) = runWriterT (runExceptT m)

runUpdateRepo :: UpdateRepo a -> IO (Either UpdateRepoError a, [String])
runUpdateRepo = runUpdateRepoT

-- | Performs a git fetch for the latest date from the @origin@ remote and
-- performs a git hard reset to the latest commit.
updateRepo :: UpdateRepo ()
updateRepo = do
  let opts1 = ["fetch", "origin"]
  (code1, out1, err1) <- liftIO $ readProcessWithExitCode "git" opts1 ""
  when (code1 /= ExitSuccess) $ do
    throwError $ FailedToFetch err1
  tell [out1]

  let opts2 = ["reset", "--hard", "origin/master"]
  (code2, out2, err2) <- liftIO $ readProcessWithExitCode "git" opts2 ""
  when (code2 /= ExitSuccess) $ do
    throwError $ FailedToReset err2
  tell [out2]

runMakefile :: UpdateRepo ()
runMakefile = do
  (code, out, err) <- liftIO $ readProcessWithExitCode "make" [] ""
  when (code /= ExitSuccess) $ do
    throwError $ FailedToMake err
  tell [out]
