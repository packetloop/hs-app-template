{-# LANGUAGE TemplateHaskell #-}
module Options where

import Control.Lens
import Control.Monad.Logger (LogLevel (..))
import Network.AWS.S3.Types (BucketName, ObjectKey, Region (..))

import qualified Network.AWS as AWS

data Options = Options
  { _optLogLevel :: LogLevel
  , _optRegion   :: Region
  }

makeLenses ''Options

awsLogLevel :: Options -> AWS.LogLevel
awsLogLevel o = case o ^. optLogLevel of
  LevelError -> AWS.Error
  LevelWarn  -> AWS.Error
  LevelInfo  -> AWS.Error
  LevelDebug -> AWS.Info
  _          -> AWS.Trace
