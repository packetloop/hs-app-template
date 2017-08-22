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

  withStdOutTimedFastLogger $ \logger -> do
    env <- mkEnv (opt ^. optRegion) (opt ^. optLogLevel) logger
    void . runApplication progName env opt logger $ do
      logInfo "Instantiating Schema Registry"
      sr <- schemaRegistry (opt ^. optKafkaSchemaRegistryAddress)

      logInfo "Creating Kafka Consumer"
      consumer <- mkConsumer opt
      -- producer <- mkProducer opt -- Use this if you also want a producer.

      logInfo "Running Kafka Consumer"
      runConduit $
        kafkaSourceNoClose consumer (opt ^. optKafkaPollTimeoutMs)
        .| throwLeftSatisfy isFatal                   -- throw any fatal error
        .| skipNonFatalExcept [isPollTimeout]         -- discard any non-fatal except poll timeouts
        .| Srv.handleStream sr                        -- handle messages (see Service.hs)
        -- .| batchByOrFlushEither (opt ^. optBatchSize) -- Use this if you also want a producer.
        .| everyNSeconds (opt ^. optKafkaConsumerCommitPeriodSec)  -- only commit ever N seconds, so we don't hammer Kafka.
        .| commitOffsetsSink consumer
        -- .| flushThenCommitSink consumer producer -- Swap with the above if you want a producer.

    pushLogMessage logger LevelError ("Premature exit, must not happen." :: String)
