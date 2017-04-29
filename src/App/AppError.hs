module App.AppError
where

import Control.Monad.Catch
import Kafka.Avro
import Kafka.Types

data AppError = KafkaErr KafkaError
              | DecodeErr DecodeError
              | AppErr String
              deriving Show
instance Exception AppError
