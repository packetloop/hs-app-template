module Service
  ( handleStream
  )
where

import Control.Arrow                        (left)
import Control.Monad.IO.Class
import Data.ByteString                      (ByteString)
import Data.ByteString.Lazy                 (fromStrict)
import Data.Conduit
import Kafka.Avro                           (SchemaRegistry, decodeWithSchema)
import Kafka.Conduit.Source

import qualified Data.Conduit.List as L

import App

-- | Handles the stream of incoming messages.
-- Change the return type to anything.
-- Emit values downstream because offsets are committed based on their present.
handleStream :: MonadApp m
             => SchemaRegistry
             -> Conduit
                  (Either KafkaError (ConsumerRecord (Maybe ByteString) (Maybe ByteString)))
                  m
                  (Either AppError ByteString)
handleStream sr =
  L.map boxErrors
  .| L.map (fmap crValue)              -- extracting only value from consumer record
  .| L.mapMaybe sequenceA              -- discard empty values
  .| L.mapM (bindM $ decodeMessage sr) -- decode avro message. Uncomment when needed.
  -- .| L.map (>>= mapAvro)            -- TODO: Implement `mapAvro` if necessary.

decodeMessage :: MonadIO m => SchemaRegistry -> ByteString -> m (Either AppError ByteString)
decodeMessage sr bs = left DecodeErr <$> decodeWithSchema sr (fromStrict bs)

bindM :: Monad m => (b -> m (Either a c)) -> Either a b -> m (Either a c)
bindM _ (Left a) = return $ Left a
bindM f (Right b) = f b
