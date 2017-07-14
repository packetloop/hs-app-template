module App.AppError
where

import Control.Monad.Catch
import Kafka.Avro
import Kafka.Types
import Data.Bifunctor (first)

data AppError = KafkaErr KafkaError
              | DecodeErr DecodeError
              | AppErr String
              deriving (Show, Eq)
instance Exception AppError

boxErrors :: Either KafkaError a -> Either AppError a
boxErrors = first KafkaErr
