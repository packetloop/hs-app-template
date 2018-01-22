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
  let logLvk  = opt ^. optLogLevel
  let statsConf = opt ^. optStatsConfig

  withStdOutTimedFastLogger $ \lgr -> do
    withStatsClient progName statsConf $ \stats -> do
      envAws <- mkEnv (opt ^. optRegion) logLvk lgr
      let envApp = AppEnv opt envAws stats (Logger lgr logLvk)
      res <- runApplication envApp
      case res of
        Left err -> pushLogMessage lgr LevelError ("Exiting: " <> show err)
        Right _  -> pure ()
    pushLogMessage lgr LevelError ("Premature exit, must not happen." :: String)


runApplication :: AppEnv -> IO (Either AppError AppState)
runApplication envApp =
  runApplicationM envApp $ do
    opt <- view appOptions
    kafkaConf <- view kafkaConfig

    logInfo "Creating Kafka Consumer"
    consumer <- mkConsumer (opt ^. consumerGroupId) (opt ^. optInputTopic)
    -- producer <- mkProducer -- Use this if you also want a producer.

    logInfo "Instantiating Schema Registry"
    sr <- schemaRegistry (kafkaConf ^. schemaRegistryAddress)

    logInfo "Running Kafka Consumer"
    runConduit $
      kafkaSourceNoClose consumer (kafkaConf ^. pollTimeoutMs)
      .| throwLeftSatisfyC KafkaErr isFatal            -- throw any fatal error
      .| skipNonFatalExcept [isPollTimeout]            -- discard any non-fatal except poll timeouts
      .| rightC (Srv.handleStream sr)                  -- handle messages (see Service.hs)
      .| everyNSeconds (kafkaConf ^. commitPeriodSec)  -- only commit ever N seconds, so we don't hammer Kafka.
      .| commitOffsetsSink consumer
      -- .| flushThenCommitSink consumer producer -- Swap with the above if you want a producer.


---------------------- TO BE MOVED TO A LIBRARY -------------------------------
throwLeftC :: MonadAppError m => (e -> AppError) -> Conduit (Either e a) m (Either e a)
throwLeftC f = awaitForever $ \msg ->
  throwErrorAs f msg

throwLeftSatisfyC :: MonadAppError m => (e -> AppError) -> (e -> Bool) -> Conduit (Either e a) m (Either e a)
throwLeftSatisfyC f p = awaitForever $ \msg ->
  case msg of
    Right a -> yield (Right a)
    Left e  | p e -> throwErrorAs f (Left e)
    Left e  -> yield (Left e)
-------------------------------------------------------------------------------

withStatsClient :: AppName -> StatsConfig -> (StatsClient -> IO a) -> IO a
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

