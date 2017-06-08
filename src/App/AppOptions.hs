{-# LANGUAGE TemplateHaskell #-}
module App.AppOptions
where

import App.Options
import Control.Lens
import Network.StatsD (StatsClient)

data AppOptions = AppOptions
  { _appOpts        :: Options
  , _appStatsClient :: StatsClient
  }

makeLenses ''AppOptions
