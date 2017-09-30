{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module App.AppEnv
where

import App.Options
import Arbor.Logger   (TimedFastLogger)
import Control.Lens
import Network.StatsD (StatsClient)

data AppEnv = AppEnv
  { _appOptions     :: Options
  , _appStatsClient :: StatsClient
  , _appLogger      :: TimedFastLogger
  }

makeClassy ''AppEnv

class HasStatsClient a where
  statsClient :: Lens' a StatsClient

instance HasStatsClient StatsClient where
  statsClient = id

instance HasStatsClient AppEnv where
  statsClient = appStatsClient

class HasLogger a where
  logger :: Lens' a TimedFastLogger

instance HasLogger AppEnv where
  logger = appLogger

instance HasKafkaConfig AppEnv where
  kafkaConfig = appEnv . kafkaConfig

instance HasStatsConfig AppEnv where
  statsConfig = appEnv . statsConfig
