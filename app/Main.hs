module Main where

import Arbor.Logger
import Control.Lens
import Control.Monad                        (void)
import Control.Monad.Trans.AWS
import Data.Conduit
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
  void . runApplication "hw-app-template" env opt $ do

    logInfo "Instantiating Schema Registry"
    sr <- schemaRegistry (opt ^. optSchemaRegistryAddress)

    logInfo "Creating Kafka Consumer"
    consumer <- mkConsumer opt

    logInfo "Running Kafka Consumer"
    runConduit $
      kafkaSourceNoClose consumer (Timeout (opt ^. optKafkaPollTimeout))
      .| throwLeftSatisfy isFatal             -- throw any fatal error
      .| skipNonFatalExcept [isPollTimeout]   -- discard any non-fatal except poll timeouts
      .| Srv.handleStream sr                  -- handle messages (see Service.hs)
      .| everyN 100                           -- after every 100 messages commit offsets
      .| commitOffsetsSink consumer

    logError "Premature exit, must not happen."

mkEnv :: Options -> IO Env
mkEnv opt = do
  lgr <- newLogger (awsLogLevel opt) stdout
  newEnv Discover <&> (envLogger .~ lgr) . set envRegion (opt ^. optRegion)
