module Service
  ( handleStream
  )
where

import Control.Monad.Catch    (MonadThrow)
import Control.Monad.IO.Class
import Data.ByteString        (ByteString)
import Data.ByteString.Lazy   (fromStrict)
import Data.Conduit
import Kafka.Avro             (SchemaRegistry, decodeWithSchema)
import Kafka.Conduit.Source

import qualified Data.Conduit.List as L

import App

-- | Handles the stream of incoming messages.
-- Change the return type to anything.
-- Emit values downstream because offsets are committed based on their present.
handleStream :: MonadApp m
             => SchemaRegistry
             -> Sink (ConsumerRecord (Maybe ByteString) (Maybe ByteString)) m ()
handleStream sr =
  L.map crValue                 -- extracting only value from consumer record
  .| L.catMaybes                -- discard empty values
  .| L.mapM (decodeMessage sr)  -- decode avro message. Uncomment when needed.
  .| L.sinkNull

decodeMessage :: (MonadIO m, MonadThrow m) => SchemaRegistry -> ByteString -> m ByteString
decodeMessage sr bs = decodeWithSchema sr (fromStrict bs) >>= throwAs DecodeErr
