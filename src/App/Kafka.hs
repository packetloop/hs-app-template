module App.Kafka
where

import App.Options
import Control.Lens                 hiding (cons)
import Control.Monad                (void)
import Control.Monad.Logger         (LogLevel (..))
import Control.Monad.Trans.Resource
import Data.Foldable
import Data.List.Split
import Data.Monoid                  ((<>))
import Kafka.Conduit.Sink           as KSnk
import Kafka.Conduit.Source         as KSrc


mkConsumer :: MonadResource m => LogLevel -> KafkaConfig -> m KafkaConsumer
mkConsumer logLevel conf =
  let props = fold
        [ KSrc.brokersList [conf ^. broker]
        , conf ^. consumerGroupId    & groupId
        , conf ^. queuedMaxMsgKBytes & queuedMaxMessagesKBytes
        , noAutoCommit
        , KSrc.suppressDisconnectLogs
        , consumerLogLevel (kafkaLogLevel logLevel)
        , KSrc.debugOptions (kafkaDebugEnable (conf ^. debugOpts))
        ]
      sub = topics [conf ^. inputTopic] <> offsetReset Earliest
      cons = newConsumer props sub >>= either throwM return
   in snd <$> allocate cons (void . closeConsumer)

mkProducer :: MonadResource m => KafkaConfig -> m KafkaProducer
mkProducer conf =
  let props = KSnk.brokersList [conf ^. broker]
           <> KSnk.suppressDisconnectLogs
      prod = newProducer props >>= either throwM return
   in snd <$> allocate prod closeProducer

kafkaLogLevel :: LogLevel -> KafkaLogLevel
kafkaLogLevel l = case l of
  LevelDebug   -> KafkaLogDebug
  LevelInfo    -> KafkaLogInfo
  LevelWarn    -> KafkaLogWarning
  LevelError   -> KafkaLogErr
  LevelOther _ -> KafkaLogCrit

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
