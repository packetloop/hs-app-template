module Service
where

import Arbor.Datadog.Conduit
import Arbor.Logger
import Control.Arrow                        (left)
import Control.Monad.IO.Class
import Data.ByteString                      (ByteString)
import Data.ByteString.Lazy                 (fromStrict)
import Data.Conduit
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Conduit.Combinator
import Kafka.Avro                           (SchemaRegistry, decodeWithSchema)
import Kafka.Conduit.Source

import qualified Data.Conduit.List as L

import App

-- | Handles the stream of incoming messages.
-- Change the return type to anything.
-- Emit values downstream because offsets are committed based on their present.
handleStream :: MonadApp m
             => SchemaRegistry
             -> Sink Metric m ()
             -> Conduit (Either KafkaError (ConsumerRecord (Maybe ByteString) (Maybe ByteString))) m ()
handleStream sr stats =
  projectRights             -- getting rid of errors
  .| L.map crValue          -- extracting only value from consumer record
  .| L.catMaybes            -- discard empty values
  .| tap (countStats (MetricName "messages.received") stats)
--  .| L.mapM (decodeMessage sr)  -- decode avro message. Uncomment when needed.
  .| L.mapM (\x -> logInfo ("Received message: " <> show x))

decodeMessage :: MonadIO m => SchemaRegistry -> ByteString -> m (Either AppError ByteString)
decodeMessage sr bs = left DecodeErr <$> decodeWithSchema sr (fromStrict bs)
