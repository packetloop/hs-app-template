{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module App.Application
where

import Arbor.Logger
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger         (LoggingT, MonadLogger)
import Control.Monad.Reader
import Control.Monad.State.Strict   (MonadState (..), StateT, execStateT)
import Control.Monad.Trans.Resource
import Data.Text                    (Text)
import Network.AWS                  as AWS hiding (LogLevel)
import Network.StatsD               as S

import App.AppEnv
import App.AppState
import App.Options
import App.Orphans  ()

type AppName = Text

class ( MonadReader AppEnv m
      , MonadState AppState m
      , MonadLogger m
      , MonadAWS m
      , MonadStats m
      , MonadResource m
      , MonadThrow m
      , MonadCatch m
      , MonadIO m) => MonadApp m where

newtype Application a = Application
  { unApp :: ReaderT AppEnv (StateT AppState (LoggingT AWS)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadBase IO
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadReader AppEnv
             , MonadState AppState
             , MonadAWS
             , MonadLogger
             , MonadResource)

deriving instance MonadApp Application

instance MonadStats Application where
  getStatsClient = reader _appStatsClient

runApplication :: HasEnv e => e -> AppEnv -> Application () -> IO AppState
runApplication envAws envApp f =
  runResourceT
    . runAWS envAws
    . runTimedLogT (envApp ^. appOptions . optLogLevel) (envApp ^. appLogger)
    . flip execStateT appStateEmpty
    $ do
        logInfo $ show (envApp ^. appOptions)
        runReaderT (unApp f) envApp

