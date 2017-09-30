{-# LANGUAGE MultiParamTypeClasses #-}
module App.Kafka
where

import App.Options
import Arbor.Logger
import Control.Lens                 hiding (cons)
import Control.Monad                (void)
import Control.Monad.Logger         (LogLevel (..))
import Control.Monad.Trans.Resource
import Data.Foldable
import Data.List.Split
import Data.Monoid                  ((<>))
import Kafka.Conduit.Sink           as KSnk
import Kafka.Conduit.Source         as KSrc


-- Can be this:
-- mkConsumer :: (MonadResource m, MonadReader r m, HasKafkaConfig r, HasLogger r) => LogLevel -> m KafkaConsumer
-- I can't decide which one is better so I leave this comment
mkConsumer :: MonadResource m => LogLevel -> TimedFastLogger -> KafkaConfig -> m KafkaConsumer
mkConsumer lvl lgr conf =
  let props = fold
        [ KSrc.brokersList [conf ^. broker]
        , conf ^. consumerGroupId    & groupId
        , conf ^. queuedMaxMsgKBytes & queuedMaxMessagesKBytes
        , noAutoCommit
        , KSrc.suppressDisconnectLogs
        , consumerLogLevel (kafkaLogLevel lvl)
        , KSrc.debugOptions (kafkaDebugEnable (conf ^. debugOpts))
        , KSrc.setCallback (logCallback   (\l s1 s2 -> pushLogMessage lgr (kafkaLogLevelToLogLevel $ toEnum l) ("[" <> s1 <> "] " <> s2)))
        , KSrc.setCallback (errorCallback (\e s -> pushLogMessage lgr LevelError ("[" <> show e <> "] " <> s)))
        ]
      sub = topics [conf ^. inputTopic] <> offsetReset Earliest
      cons = newConsumer props sub >>= either throwM return
   in snd <$> allocate cons (void . closeConsumer)

mkProducer :: MonadResource m => LogLevel -> TimedFastLogger -> KafkaConfig -> m KafkaProducer
mkProducer lvl lgr conf =
  let props = KSnk.brokersList [conf ^. broker]
           <> KSnk.suppressDisconnectLogs
           <> logLevel (kafkaLogLevel lvl)
           <> KSnk.setCallback (logCallback   (\l s1 s2 -> pushLogMessage lgr (kafkaLogLevelToLogLevel $ toEnum l) ("[" <> s1 <> "] " <> s2)))
           <> KSnk.setCallback (errorCallback (\e s -> pushLogMessage lgr LevelError ("[" <> show e <> "] " <> s)))
      prod = newProducer props >>= either throwM return
   in snd <$> allocate prod closeProducer

kafkaLogLevel :: LogLevel -> KafkaLogLevel
kafkaLogLevel l = case l of
  LevelDebug   -> KafkaLogDebug
  LevelInfo    -> KafkaLogInfo
  LevelWarn    -> KafkaLogWarning
  LevelError   -> KafkaLogErr
  LevelOther _ -> KafkaLogCrit

kafkaLogLevelToLogLevel :: KafkaLogLevel -> LogLevel
kafkaLogLevelToLogLevel l = case l of
  KafkaLogDebug   -> LevelDebug
  KafkaLogInfo    -> LevelInfo
  KafkaLogWarning -> LevelWarn
  KafkaLogErr     -> LevelError
  KafkaLogCrit    -> LevelError
  KafkaLogAlert   -> LevelWarn
  KafkaLogNotice  -> LevelInfo
  KafkaLogEmerg   -> LevelError

kafkaDebugEnable :: String -> [KafkaDebug]
kafkaDebugEnable str = map debug (splitWhen (== ',') str)
  where
    debug :: String -> KafkaDebug
    debug m = case m of
      "generic"  -> DebugGeneric
      "broker"   -> DebugBroker
      "topic"    -> DebugTopic
      "metadata" -> DebugMetadata
      "queue"    -> DebugQueue
      "msg"      -> DebugMsg
      "protocol" -> DebugProtocol
      "cgrp"     -> DebugCgrp
      "security" -> DebugSecurity
      "fetch"    -> DebugFetch
      "feature"  -> DebugFeature
      "all"      -> DebugAll
      _          -> DebugGeneric
