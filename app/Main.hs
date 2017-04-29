module Main where

import Arbor.Datadog.Conduit
import Arbor.Logger
import Control.Lens
import Control.Monad                        (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Data.Conduit
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Conduit.Combinator
import Kafka.Avro                           (schemaRegistry)
import Kafka.Conduit.Source
import System.IO                            (stdout)

import App
import App.Kafka
import Service   as Srv


main :: IO ()
main = do
  opt <- parseOptions
  env <- mkEnv opt
  void . runApplication env opt $ do
    logInfo $ show opt

    logInfo "Instantiating Schema Registry"
    sr <- schemaRegistry (opt ^. optSchemaRegistryAddress)

    logInfo "Creating Kafka Consumer"
    consumer <- mkConsumer opt

    logInfo "Instantiating StatsD client"
    stats <- mkStatsClient (DogStatsSettings (opt ^. optStatsdHost) (opt ^. optStatsdPort))
    stags <- liftIO $ statsTags opt
    let statsSink = metricSink (MetricName "hw-app-template") (toTags stags) (SampleRate 1.0) stats

    logInfo "Running Kafka Consumer"
    runConduit $
      kafkaSourceNoClose consumer (Timeout (opt ^. optKafkaPollTimeout))
      .| throwLeftSatisfy isFatal             -- throw any fatal error
      .| skipNonFatalExcept [isPollTimeout]   -- discard any non-fatal except poll timeouts
      .| Srv.handleStream sr statsSink        -- handle messages (see Service.hs)
      .| everyN 100                           -- after every 100 messages commit offsets
      .| commitOffsetsSink consumer

    logError "Premature exit, must not happen."

statsTags :: MonadIO m => Options -> m [StatsTag]
statsTags opts = do
  deplId <- liftIO $ envTag "TASK_DEPLOY_ID" "deploy_id"
  return $ deplId <> opts ^. optStatsdTags

mkEnv :: Options -> IO Env
mkEnv opt = do
  lgr <- newLogger (awsLogLevel opt) stdout
  newEnv Discover <&> (envLogger .~ lgr) . set envRegion (opt ^. optRegion)
