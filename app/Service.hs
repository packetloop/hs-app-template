module Service
  ( handleStream
  )
where

import Conduit
import Data.ByteString      (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Kafka.Avro           (SchemaRegistry, decodeWithSchema)
import Kafka.Conduit.Source

import qualified Data.Conduit.List as L

import App

-- | Handles the stream of incoming messages.
-- Change the return type to anything.
-- Emit values downstream because offsets are committed based on their present.
handleStream :: MonadApp m
             => SchemaRegistry
             -> Conduit (ConsumerRecord (Maybe ByteString) (Maybe ByteString)) m ()
handleStream sr =
  mapC crValue                 -- extracting only value from consumer record
  .| L.catMaybes                -- discard empty values
  .| mapMC (decodeMessage sr)  -- decode avro message. Uncomment when needed.
  .| mapC (const ())

decodeMessage :: (MonadIO m, MonadAppError m) => SchemaRegistry -> ByteString -> m ByteString
decodeMessage sr bs = decodeWithSchema sr (fromStrict bs) >>= throwErrorAs DecodeErr
