module Main where

import App
import App.Kafka
import Arbor.Logger
import Control.Lens
import Control.Monad                        (void)
import Control.Monad.Trans.AWS
import Data.Conduit
import HaskellWorks.Data.Conduit.Combinator
import Kafka.Avro                           (schemaRegistry)
import Kafka.Conduit.Source
import Kafka.Conduit.Sink
import System.Environment
import System.IO                            (stdout)

import qualified Data.Text as T
import qualified Service   as Srv

main :: IO ()
main = do
  opt <- parseOptions
  env <- mkEnv opt
  progName <- T.pack <$> getProgName
  void . runApplication progName env opt $ do
    logInfo "Instantiating Schema Registry"
    sr <- schemaRegistry (opt ^. optKafkaSchemaRegistryAddress)

    logInfo "Creating Kafka Consumer"
    consumer <- mkConsumer opt
    -- producer <- mkProducer opt -- Use this if you also want a producer.

    logInfo "Running Kafka Consumer"
    runConduit $
      kafkaSourceNoClose consumer (Timeout $ opt ^. optKafkaPollTimeoutMs)
      .| throwLeftSatisfy isFatal                   -- throw any fatal error
      .| skipNonFatalExcept [isPollTimeout]         -- discard any non-fatal except poll timeouts
      .| Srv.handleStream sr                        -- handle messages (see Service.hs)
      -- .| batchByOrFlushEither (opt ^. optBatchSize) -- Use this if you also want a producer.
      .| everyNSeconds (opt ^. optKafkaConsumerCommitPeriodSec)  -- only commit ever N seconds, so we don't hammer Kafka.
      .| commitOffsetsSink consumer
      -- .| flushThenCommitSink consumer producer -- Swap with the above if you want a producer.


    logError "Premature exit, must not happen."

mkEnv :: Options -> IO Env
mkEnv opt = do
  lgr <- newLogger (awsLogLevel opt) stdout
  newEnv Discover <&> (envLogger .~ lgr) . set envRegion (opt ^. optRegion)
