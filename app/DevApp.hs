{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module DevApp
where

import Arbor.Logger
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger         (LoggingT, MonadLogger)
import Control.Monad.Trans.Resource
import Network.AWS                  as AWS hiding (LogLevel)
import Network.StatsD               as S

import App.AWS.Env
import App.Orphans ()

newtype DevApp a = DevApp
  { unDevApp :: (LoggingT AWS) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow, MonadCatch
             , MonadMask, MonadAWS, MonadLogger, MonadResource)

instance MonadStats DevApp where
  getStatsClient = pure Dummy

runDevApp' :: HasEnv e => e -> TimedFastLogger -> DevApp a -> IO a
runDevApp' e logger f = runResourceT . runAWS e $ runTimedLogT LevelInfo logger (unDevApp f)


runDevApp :: DevApp a -> IO a
runDevApp f =
  withStdOutTimedFastLogger $ \logger -> do
    env <- mkEnv Oregon LevelInfo logger
    runDevApp' env logger f
