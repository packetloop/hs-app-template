module Service
where

import Arbor.Datadog.Conduit
import Arbor.Logger
import Data.ByteString                      (ByteString)
import Data.Conduit
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Conduit.Combinator
import Kafka.Avro
import Kafka.Conduit.Source

import qualified Data.Conduit.List as L

import App
import App.Kafka

-- | Handles the stream of incoming messages.
-- Change the return type to anything.
-- Emit values downstream because offsets are committed based on their present.
handleStream :: MonadApp m
             => Sink Metric m ()
             -> Conduit (Either KafkaError (ConsumerRecord (Maybe ByteString) (Maybe ByteString))) m ()
handleStream stats =
  projectRights             -- getting rid of errors
  .| L.map crValue          -- extracting only value from consumer record
  .| L.catMaybes            -- discard empty values
  .| tap (countStats (MetricName "messages.received") stats)
  .| L.mapM (\x -> logInfo ("Received message: " <> show x))
