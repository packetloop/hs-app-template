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
import Kafka.Conduit


mkConsumer :: MonadResource m => Options -> m KafkaConsumer
mkConsumer opts =
  let props = fold
        [ consumerBrokersList [opts ^. optKafkaBroker]
        , opts ^. optKafkaGroupId                 & groupId
        , opts ^. optKafkaQueuedMaxMessagesKBytes & consumerQueuedMaxMessagesKBytes
        , noAutoCommit
        , consumerSuppressDisconnectLogs
        , consumerLogLevel (kafkaLogLevel (opts ^. optLogLevel))
        , consumerDebug (kafkaDebugEnable (opts ^. optKafkaDebugEnable))
        ]
      sub = topics [opts ^. optCommandsTopic] <> offsetReset Earliest
      cons = newConsumer props sub >>= either throwM return
   in snd <$> allocate cons (void . closeConsumer)

mkProducer :: MonadResource m => Options -> m KafkaProducer
mkProducer opts =
  let props = producerBrokersList [opts ^. optKafkaBroker]
              <> producerSuppressDisconnectLogs
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
