module Main where

import App
import App.AWS.Env
import App.Kafka
import Arbor.Logger
import Control.Lens
import Control.Monad                        (void)
import Data.Conduit
import HaskellWorks.Data.Conduit.Combinator
import Kafka.Avro                           (schemaRegistry)
import Kafka.Conduit.Sink
import Kafka.Conduit.Source
import System.Environment

import qualified Data.Text as T
import qualified Service   as Srv

main :: IO ()
main = do
  opt <- parseOptions
  progName <- T.pack <$> getProgName
  let logLevel = opt ^. optLogLevel

  withStdOutTimedFastLogger $ \logger -> do
    env <- mkEnv (opt ^. optRegion) logLevel logger
    void . runApplication progName env opt logger $ do
      logInfo "Creating Kafka Consumer"
      let kafkaConf = opt ^. optKafkaConfig

      consumer <- mkConsumer logLevel kafkaConf
      -- producer <- mkProducer kafkaConf -- Use this if you also want a producer.

      logInfo "Instantiating Schema Registry"
      sr <- schemaRegistry (kafkaConf ^. schemaRegistryAddress)

      logInfo "Running Kafka Consumer"
      runConduit $
        kafkaSourceNoClose consumer (kafkaConf ^. pollTimeoutMs)
        .| throwLeftSatisfy isFatal                   -- throw any fatal error
        .| skipNonFatalExcept [isPollTimeout]         -- discard any non-fatal except poll timeouts
        .| tapRight (Srv.handleStream sr)             -- handle messages (see Service.hs)
        -- .| batchByOrFlushEither (kafkaConf ^. batchSize) -- Use this if you also want a producer.
        .| everyNSeconds (kafkaConf ^. commitPeriodSec)  -- only commit ever N seconds, so we don't hammer Kafka.
        .| commitOffsetsSink consumer
        -- .| flushThenCommitSink consumer producer -- Swap with the above if you want a producer.

    pushLogMessage logger LevelError ("Premature exit, must not happen." :: String)

-- withStatsClient :: AppName -> Options -> (StatsClient -> IO ()) -> IO ()
-- withStatsClient opt = do
--   globalTags <- statsTags opt
