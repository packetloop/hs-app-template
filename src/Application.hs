{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Application
where

import Arbor.Logger
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger         (LoggingT, MonadLogger)
import Control.Monad.Reader
import Control.Monad.State.Strict   (MonadState (..), StateT, execStateT, lift)
import Control.Monad.Trans.Resource
import Data.Conduit
import Network.AWS                  as AWS hiding (LogLevel)

import AppState
import Options


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
             , MonadState AppState
             , MonadAWS
             , MonadLogger
             , MonadResource)

runApplication :: HasEnv e => e -> Options -> Application () -> IO AppState
runApplication e opt val =
  runResourceT
  . runAWS e
  . runLogT' (opt ^. optLogLevel)
  . flip execStateT appStateEmpty
  $ runReaderT (unApp val) opt

instance MonadAWS m => MonadAWS (LoggingT m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ConduitM i o m) where liftAWS = lift . liftAWS
