module App.AWS.Env
( mkEnv
, AWS.HasEnv
)
where

import           Arbor.Logger
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.AWS hiding (LogLevel)
import           Data.ByteString.Builder
import qualified Network.AWS             as AWS

mkEnv :: Region -> LogLevel -> TimedFastLogger -> IO AWS.Env
mkEnv region logLevel logger = do
  lgr <- newAwsLogger logLevel logger
  newEnv Discover <&> (envLogger .~ lgr) . set envRegion region

newAwsLogger :: Monad m => LogLevel -> TimedFastLogger -> m AWS.Logger
newAwsLogger logLevel logger = return $ \y b ->
  when (logLevelToAWS logLevel >= y)
    $ pushLogMessage logger logLevel (toLazyByteString b)

logLevelToAWS :: LogLevel -> AWS.LogLevel
logLevelToAWS l = case l of
  LevelError -> AWS.Error
  LevelWarn  -> AWS.Error
  LevelInfo  -> AWS.Error
  LevelDebug -> AWS.Info
  _          -> AWS.Trace
