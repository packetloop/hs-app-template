{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module App.AppEnv
where

import App.Options
import Control.Lens
import Kafka.Consumer.Types
import Kafka.Types
import Network.StatsD       (SampleRate, StatsClient)

-- data Stats = Stats
--   { _statsClient     :: StatsClient
--   , _statsSampleRate :: SampleRate
--   }

data AppEnv = AppEnv
  { _appEnvOptions     :: Options
  , _appEnvStatsClient :: StatsClient
  }

-- makeClassy ''Stats
makeClassy ''AppEnv
