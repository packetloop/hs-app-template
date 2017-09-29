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
