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
import Data.Maybe                   (catMaybes)
import Data.Semigroup               ((<>))
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
  getStatsClient = reader _appEnvStatsClient

runApplication :: HasEnv e => AppName -> e -> Options -> TimedFastLogger -> Application () -> IO AppState
runApplication appName e opt logger val =
  runResourceT
    . runAWS e
    . runTimedLogT (opt ^. optLogLevel) logger
    . flip execStateT appStateEmpty
    $ do
        logInfo $ show opt

        logInfo "Instantiating StatsD client"
        globalTags <- mkStatsTags opt
        let statsConf = opt ^. optStatsConfig
        let statsOpts = DogStatsSettings (statsConf ^. statsHost) (statsConf ^. statsPort)
        (_, stats) <- allocate (createStatsClient statsOpts (MetricName appName) globalTags) closeStatsClient

        runReaderT (unApp val) (AppEnv opt stats)

mkStatsTags :: MonadIO m => Options -> m [Tag]
mkStatsTags opts = liftIO $ do
  deplId <- envTag "TASK_DEPLOY_ID" "deploy_id"
  let envTags = catMaybes [deplId]
  return $ envTags <> (opts ^. optStatsConfig . statsTags <&> toTag)

toTag :: StatsTag -> Tag
toTag (StatsTag (k, v)) = S.tag k v
{-# INLINE toTag #-}
