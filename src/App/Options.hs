{-# LANGUAGE TemplateHaskell #-}
module App.Options where

import Control.Lens
import Control.Monad.Logger  (LogLevel (..))
import Data.Semigroup        ((<>))
import Network.AWS.Data.Text (FromText (..), fromText)
import Network.AWS.S3.Types  (Region (..))
import Network.Socket        (HostName)
import Network.StatsD        (SampleRate (..))
import Options.Applicative
import Text.Read             (readEither)

import Kafka.Consumer.Types
import Kafka.Types

import           Data.Text   (Text)
import qualified Data.Text   as T
import qualified Network.AWS as AWS

newtype StatsTag = StatsTag (Text, Text) deriving (Show, Eq)

data Options = Options
  { _optLogLevel                     :: LogLevel
  , _optRegion                       :: Region
  , _optKafkaBroker                  :: BrokerAddress
  , _optKafkaSchemaRegistryAddress   :: String
  , _optKafkaPollTimeoutMs           :: Int
  , _optKafkaQueuedMaxMessagesKBytes :: Int
  , _optKafkaGroupId                 :: ConsumerGroupId
  , _optCommandsTopic                :: TopicName
  , _optStatsdHost                   :: HostName
  , _optStatsdPort                   :: Int
  , _optStatsdTags                   :: [StatsTag]
  , _optSampleRate                   :: SampleRate
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
  <*> ( BrokerAddress <$> strOption
        (  long "kafka-broker"
        <> short 'b'
        <> metavar "ADDRESS:PORT"
        <> help "Kafka bootstrap broker"
        ))
  <*> strOption
        (  long "kafka-schema-registry"
        <> short 'r'
        <> metavar "HTTP_URL:PORT"
        <> help "Schema registry address")
  <*> readOption
        (  long "kafka-poll-timeout-ms"
        <> short 'u'
        <> metavar "KAFKA_POLL_TIMEOUT_MS"
        <> showDefault <> value 1000
        <> help "Kafka poll timeout (in milliseconds)")
  <*> readOption
        (  long "kafka-queued-max-messages-kbytes"
        <> short 'q'
        <> metavar "KAFKA_QUEUED_MAX_MESSAGES_KBYTES"
        <> showDefault <> value 100000
        <> help "Kafka queued.max.messages.kbytes")
  <*> ( ConsumerGroupId <$> strOption
        (  long "kafka-group-id"
        <> short 'g'
        <> metavar "GROUP_ID"
        <> help "Kafka consumer group id"))
  <*> ( TopicName <$> strOption
        (  long "commands-topic"
        <> short 'i'
        <> metavar "TOPIC"
        <> help "Commands topic"))
  <*> strOption
        (  long "statsd-host"
        <> short 's'
        <> metavar "HOST_NAME"
        <> showDefault <> value "127.0.0.1"
        <> help "StatsD host name or IP address")
  <*> readOption
        (  long "statsd-port"
        <> short 'p'
        <> metavar "PORT"
        <> showDefault <> value 8125
        <> help "StatsD port"
        <> hidden)
  <*> ( string2Tags <$> strOption
        (  long "statsd-tags"
        <> short 't'
        <> metavar "TAGS"
        <> showDefault <> value []
        <> help "StatsD tags"))
  <*> ( SampleRate <$> readOption
        (  long "statsd-sample-rate"
        <> short 'a'
        <> metavar "SAMPLE_RATE"
        <> showDefault <> value 0.01
        <> help "StatsD sample rate"))

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
readOptionMsg msg = option $ eitherReader (either (Left . const msg) Right . readEither)

readOrFromTextOption :: (Read a, FromText a) => Mod OptionFields a -> Parser a
readOrFromTextOption =
  let fromStr s = readEither s <|> fromText (T.pack s)
  in option $ eitherReader fromStr

string2Tags :: String -> [StatsTag]
string2Tags s = StatsTag . splitTag <$> splitTags
  where
    splitTags = T.split (==',') (T.pack s)
    splitTag t = T.drop 1 <$> T.break (==':') t

optionsParser :: ParserInfo Options
optionsParser = info (helper <*> options)
  (  fullDesc
  <> progDesc "For each attack caclulates its spuriousity index [0..1]"
  <> header "Spurious Attacks Detector"
  )

parseOptions :: IO Options
parseOptions = execParser optionsParser
