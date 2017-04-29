{-# OPTIONS_GHC -fno-warn-orphans #-}
module App.Orphans where

import Control.Monad.Logger       (LoggingT)
import Control.Monad.State.Strict (lift)
import Data.Conduit
import Network.AWS                as AWS hiding (LogLevel)

instance MonadAWS m => MonadAWS (LoggingT m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ConduitM i o m) where liftAWS = lift . liftAWS
