module Main where

import App
import App.AWS.Env
import App.Kafka
import Arbor.Logger
import Control.Exception
import Control.Lens
import Control.Monad                        (void)
import Data.Conduit
import Data.Maybe                           (catMaybes)
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Conduit.Combinator
import Kafka.Avro                           (schemaRegistry)
import Kafka.Conduit.Sink                   hiding (logLevel)
import Kafka.Conduit.Source
import Network.StatsD                       as S
import System.Environment

import qualified Data.Text as T
import qualified Service   as Srv

main :: IO ()
main = do
  opt <- parseOptions
  progName <- T.pack <$> getProgName
  let logLevel  = opt ^. optLogLevel
  let kafkaConf = opt ^. optKafkaConfig
  let statsConf = opt ^. optStatsConfig

  withStdOutTimedFastLogger $ \lgr -> do
    withStatsClient progName statsConf $ \stats -> do
      envAws <- mkEnv (opt ^. optRegion) logLevel lgr
      let envApp = AppEnv opt stats lgr

      void . runApplication envAws envApp $ do
        logInfo "Creating Kafka Consumer"
        consumer <- mkConsumer logLevel lgr kafkaConf
        -- producer <- mkProducer logLevel lgr kafkaConf -- Use this if you also want a producer.

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

    pushLogMessage lgr LevelError ("Premature exit, must not happen." :: String)

withStatsClient :: AppName -> StatsConfig -> (StatsClient -> IO ()) -> IO ()
withStatsClient appName statsConf f = do
  globalTags <- mkStatsTags statsConf
  let statsOpts = DogStatsSettings (statsConf ^. statsHost) (statsConf ^. statsPort)
  bracket (createStatsClient statsOpts (MetricName appName) globalTags) closeStatsClient f

mkStatsTags :: StatsConfig -> IO [Tag]
mkStatsTags statsConf = do
  deplId <- envTag "TASK_DEPLOY_ID" "deploy_id"
  let envTags = catMaybes [deplId]
  return $ envTags <> (statsConf ^. statsTags <&> toTag)
  where
    toTag (StatsTag (k, v)) = S.tag k v

