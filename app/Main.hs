module Main where

import Arbor.Datadog.Conduit
import Arbor.Logger
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Data.Conduit
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Conduit.Combinator
import Kafka.Avro                           (SchemaRegistry, decodeWithSchema, schemaRegistry)
import Kafka.Conduit.Source
import Network.AWS.S3.Types                 (Region (..))
import System.IO                            (stdout)

import qualified Data.Conduit.List as L

import App
import App.Kafka
import Service


main :: IO ()
main = do
  opt <- parseOptions
  env <- mkEnv opt
  _ <- runGeoMute opt env
  putStrLn "Exiting."

runGeoMute :: HasEnv e => Options -> e -> IO AppState
runGeoMute opt env = runApplication env opt $ do
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
    .| throwLeftSatisfy isFatal
    .| skipNonFatalExcept [isPollTimeout]
    .| handleStream statsSink
    .| everyN 100
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
