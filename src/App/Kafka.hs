module App.Kafka
where

import App.Options
import Control.Lens                 hiding (cons)
import Control.Monad                (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Monoid                  ((<>))
import Kafka.Conduit


mkConsumer :: MonadResource m => Options -> m KafkaConsumer
mkConsumer opts =
  let props = consumerBrokersList [opts ^. optKafkaBroker]
              <> groupId (opts ^. optGroupId)
              <> noAutoCommit
              <> consumerSuppressDisconnectLogs
              <> consumerQueuedMaxMessagesKBytes (opts ^. optKafkaQueuedMaxMessagesKBytes)
      sub = topics [opts ^. optCommandsTopic] <> offsetReset Earliest
      cons = newConsumer props sub >>= either throwM return
   in snd <$> allocate cons (void . closeConsumer)

mkProducer :: MonadResource m => Options -> m KafkaProducer
mkProducer opts =
  let props = producerBrokersList [opts ^. optKafkaBroker]
              <> producerSuppressDisconnectLogs
      prod = newProducer props >>= either throwM return
   in snd <$> allocate prod closeProducer

commitOffsetsSink :: MonadIO m => KafkaConsumer -> Sink i m ()
commitOffsetsSink c = awaitForever $ \_ -> commitAllOffsets OffsetCommit c
