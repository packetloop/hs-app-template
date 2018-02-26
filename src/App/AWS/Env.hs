module App.AWS.Env
  ( mkEnv
  , AWS.HasEnv
  ) where

import Arbor.Logger
import Control.Lens
import Control.Monad
import Control.Monad.Trans.AWS hiding (LogLevel)
import Data.ByteString.Builder
import Network.HTTP.Client     (HttpException (..), HttpExceptionContent (..))

import qualified Network.AWS as AWS

mkEnv :: Region -> LogLevel -> TimedFastLogger -> IO AWS.Env
mkEnv region logLevel logger = do
  lgr <- newAwsLogger logLevel logger
  newEnv Discover
    <&> envLogger .~ lgr
    <&> envRegion .~ region
    <&> envRetryCheck .~ retryCheckPolicy 20
    <&> override (over serviceRetry setRetryParams)
  where setRetryParams retry = retry & retryAttempts .~ 20

newAwsLogger :: Monad m => LogLevel -> TimedFastLogger -> m AWS.Logger
newAwsLogger logLevel logger = return $ \y b ->
  when (logLevelToAWS logLevel >= y)
    $ pushLogMessage logger logLevel (toLazyByteString b)

retryCheckPolicy :: Int -> Int -> HttpException -> Bool
retryCheckPolicy maxNum attempt ex = (attempt <= maxNum) && shouldRetry ex

logLevelToAWS :: LogLevel -> AWS.LogLevel
logLevelToAWS l = case l of
  LevelError -> AWS.Error
  LevelWarn  -> AWS.Error
  LevelInfo  -> AWS.Error
  LevelDebug -> AWS.Info
  _          -> AWS.Trace

shouldRetry :: HttpException -> Bool
shouldRetry ex = case ex of
  HttpExceptionRequest _ ctx -> case ctx of
    ResponseTimeout          -> True
    ConnectionTimeout        -> True
    ConnectionFailure _      -> True
    InvalidChunkHeaders      -> True
    ConnectionClosed         -> True
    InternalException _      -> True
    NoResponseDataReceived   -> True
    ResponseBodyTooShort _ _ -> True
    _                        -> False
  _ -> False

