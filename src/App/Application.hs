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
import Network.AWS                  as AWS hiding (LogLevel)

import App.AppState
import App.Options
import App.Orphans  ()

class ( MonadReader Options m
      , MonadState AppState m
      , MonadLogger m
      , MonadAWS m
      , MonadResource m
      , MonadThrow m
      , MonadCatch m
      , MonadIO m) => MonadApp m where

newtype Application a = Application
  { unApp :: ReaderT Options (StateT AppState (LoggingT AWS)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadBase IO
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadReader Options
             , MonadState AppState
             , MonadAWS
             , MonadLogger
             , MonadResource)

deriving instance MonadApp Application

runApplication :: HasEnv e => e -> Options -> Application () -> IO AppState
runApplication e opt val =
  runResourceT
  . runAWS e
  . runLogT' (opt ^. optLogLevel)
  . flip execStateT appStateEmpty
  $ runReaderT (unApp val) opt
