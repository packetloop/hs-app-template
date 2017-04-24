{-# LANGUAGE TemplateHaskell #-}
module App.Options where

import Control.Lens
import Control.Monad.Logger  (LogLevel (..))
import Data.Semigroup        ((<>))
import Network.AWS.Data.Text (FromText (..), fromText)
import Network.AWS.S3.Types  (Region (..))
import Options.Applicative
import Text.Read             (readEither)

import Kafka.Consumer.Types
import Kafka.Types

import qualified Data.Text   as T
import qualified Network.AWS as AWS

data Options = Options
  { _optLogLevel              :: LogLevel
  , _optRegion                :: Region
  , _optKafkaBroker           :: BrokerAddress
  , _optCommandsTopic         :: TopicName
  , _optGroupId               :: ConsumerGroupId
  , _optSchemaRegistryAddress :: String
  , _optKafkaPollTimeout      :: Int
  } deriving (Show)

makeLenses ''Options

options :: Parser Options
options = Options
  <$> readOptionMsg "Valid values are LevelDebug, LevelInfo, LevelWarn, LevelError"
       (  long "log-level"
       <> short 'l'
       <> metavar "LOG_LEVEL"
       <> showDefault <> value LevelInfo
       <> help "Log level.")
  <*> readOrFromTextOption
      (  long "region"
      <> short 'r'
      <> metavar "AWS_REGION"
      <> showDefault <> value Oregon
      <> help "The AWS region in which to operate"
      )
  <*> (BrokerAddress <$> strOption
        (  long "bootstrap-broker"
        <> short 'b'
        <> metavar "ADDRESS:PORT"
        <> help "Kafka bootstrap broker"
        ))
  <*>  (TopicName <$> strOption
        (  long "commands-topic"
        <> short 'i'
        <> metavar "TOPIC"
        <> help "Commands topic"))
  <*> (ConsumerGroupId <$> strOption
        (  long "group-id"
        <> short 'g'
        <> metavar "GROUP_ID"
        <> help "Kafka consumer group id"
        ))
  <*> strOption
        (  long "schema-registry"
        <> short 'r'
        <> metavar "HTTP_URL:PORT"
        <> help "Schema registry address"
        )
  <*> readOption
       (  long "poll-timeout"
       <> short 'u'
       <> metavar "KAFKA_POLL_TIMEOUT"
       <> showDefault <> value 1000
       <> help "Kafka poll timeout")

awsLogLevel :: Options -> AWS.LogLevel
awsLogLevel o = case o ^. optLogLevel of
  LevelError -> AWS.Error
  LevelWarn  -> AWS.Error
  LevelInfo  -> AWS.Error
  LevelDebug -> AWS.Info
  _          -> AWS.Trace

readOption :: Read a => Mod OptionFields a -> Parser a
readOption = option $ eitherReader readEither

readOptionMsg :: Read a => String -> Mod OptionFields a -> Parser a
readOptionMsg msg = option $ (eitherReader ((either (Left . const msg) (Right . id)) . readEither))

readOrFromTextOption :: (Read a, FromText a) => Mod OptionFields a -> Parser a
readOrFromTextOption =
  let fromStr s = readEither s <|> fromText (T.pack s)
  in option $ eitherReader fromStr

optionsParser :: ParserInfo Options
optionsParser = info (helper <*> options)
  (  fullDesc
  <> progDesc "For each attack caclulates its spuriousity index [0..1]"
  <> header "Spurious Attacks Detector"
  )

parseOptions :: IO Options
parseOptions = execParser optionsParser
