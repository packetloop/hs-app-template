module App.AWS.S3
( downloadLBS
, putFile, copySingle
, BucketName(..)
, ObjectKey(..)
, ETag(..)
, s3UriString
) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.AWS      hiding (send)
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy         (ByteString)
import Data.Conduit.Binary          (sinkLbs)
import Data.Monoid                  ((<>))
import Data.Text                    (unpack)
import Network.AWS                  (MonadAWS, send)
import Network.AWS.Data
import Network.AWS.S3

chunkSize :: ChunkSize
chunkSize = ChunkSize (1024*1024)

downloadLBS :: (MonadResource m, MonadAWS m)
            => BucketName
            -> ObjectKey
            -> m ByteString
downloadLBS bucketName objectKey = do
  resp <- send $ getObject bucketName objectKey
  (resp ^. gorsBody) `sinkBody` sinkLbs

s3UriString :: BucketName -> ObjectKey -> String
s3UriString (BucketName b) (ObjectKey k) =
  unpack $ "s3://" <> b <> "/" <> k

-- | Puts file into a specified S3 bucket
putFile :: MonadAWS m
        => BucketName       -- ^ Target bucket
        -> ObjectKey        -- ^ File name on S3
        -> FilePath         -- ^ Source file path
        -> m (Maybe ETag)   -- ^ Etag when the operation is successful
putFile b k f = do
    req <- chunkedFile chunkSize f
    view porsETag <$> send (putObject b k req)

-- | Copies a single object within S3
copySingle :: MonadAWS m
           => BucketName          -- ^ Source bucket name
           -> ObjectKey           -- ^ Source key
           -> BucketName          -- ^ Target bucket name
           -> ObjectKey           -- ^ Target key
           -> m ()
copySingle sb sk tb tk =
  void . send $ copyObject tb (toText sb <> "/" <> toText sk) tk
     & coMetadataDirective .~ Just MDCopy
