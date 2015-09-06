{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      : Network.AWS.S3.Encryption.PutObject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.PutObject where

import           Control.Arrow
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Coerce
import           Data.Conduit
import           Data.Proxy
import           Network.AWS
import           Network.AWS.Prelude                hiding (coerce)
import           Network.AWS.S3
import qualified Network.AWS.S3                     as S3
import           Network.AWS.S3.Encryption.Envelope
import           Network.AWS.S3.Encryption.Types
import           System.IO



-- metadata:
--   Put:
--     encrypt k (putObject "bkt" "key" body) >>= send
--
--   Get:
--     send (getObject "bkt" "key") >>= decrypt k
--
--   Multipart:
--     e  <- encrypt k (createMultipartUpload "bkt" "key")
--     rs <- send e
--     send $ encryptPart e (uploadPart "foo" bar (rs ^. uploadId))

-- instructions:
--   Put:
--     (i, po) <- encryptUsing k (putObject "bkt" "key" body)
--     send po
--     send (i & suffix .~ "foo")
--
--   Get:
--     decryptUsing k
--         <$> send (getInstructions "bkt" "key")
--         <*> send (getObject "bkt" "key")

-- possible:
--     encrypt k (putObject "bkt" "key")
--     decrypt k (getObject "bkt" "key")

--     (rs, f) <- initiate k (createMultipartUpload "bkt" "key")
--     send $ f (uploadPart "foo" "bar" (rs ^. uploadId))

--     encryptUsing k (Instr ".instruction") (putObject "bkt" "key")
--     decryptUsing k defaultSuffix (getObject "bkt" "key")

--     encry

-- decrypt :: (MonadResource m, MonadReader r m, HasEnv r, Decrypt a)
--         => Key
--         -> a
--         -> m a
-- decrypt k x = do
--     e' <- view environment
--     e  <- fromMetadata e k (metadata x)
--     return (decryptWith e x)

-- class Decrypt a where
--     decryptWith :: Envelope -> a -> a
--     metadata    ::             a -> HashMap Text Text

-- instance Decrypt GetObjectResponse where
--     decryptWith e = gorsBody %~ bodyDecrypt e
--     metadata      = view gorsMetadata

-- encrypt :: (MonadResource m, MonadReader r m, HasEnv r, Encrypt a)
--         => Key
--         -> a
--         -> m (Encrypted a)
-- encrypt k x = do
--     e <- view environment
--     encryptWith x Metadata <$> newEnvelope e k

encrypt :: Key -> PutObject -> m (Rs PutObject))
encrypt k x = do
    e' <- view environment
    e  <- newEnvelope e k
    send (encryptWith x Metadata e)

encryptUsing :: Key -> Maybe Text -> PutObject -> m (Rs PutObject, Rs PutObject)
encryptUsing k l x = do
    e' <- view environment
    e  <- newEnvelope e' k
    let (b, o) = instructions x
        suf    = fromMaybe instructionSuffix l
    (,) <$> send (putObject b (o <> suf) (toBody e))
        <*> send (encryptWith x Instructions e)

decrypt :: Key -> GetObject -> m (Rs GetObject)
decrypt k x = do
    e' <- view environment
    rs <- send x
    e  <- fromMetadata e' k (rs ^. gorsMetadata)
    return (rs & gorsBody %~ bodyDecrypt e)

decryptUsing :: Key -> GetObject -> m (Rs GetObject, Rs GetObject)
decryptUsing = undefined

-- | Note about parallelism/concurrency, and encryption of parts. If you don't
-- encrypt any of the parts then the entire thing is unencrypted!
initiate :: Key
         -> CreateMultipartUpload
         -> m ( Rs CreateMultipartUpload
              , UploadPart -> Encrypted UploadPart
              )
initiate = undefined

initiateUsing :: Key
              -> CreateMultipartUpload
              -> m ( Rs PutObject
                   , Rs CreateMultipartUpload
                   , UploadPart -> Encrypted UploadPart
                   )
initiateUsing = undefined

data Encrypted a = Encrypted a [Header] Location Envelope

instance AWSRequest a => AWSRequest (Encrypted a) where
    type Rs (Encrypted a) = Rs a

    request (Encrypted x hs l e) = coerce (request x)
        & rqHeaders <>~ headers
        & rqBody %~ encrypt
      where
        encrypt b
            | contentLength b > 0 = bodyEncrypt e b
            | otherwise           = b

        headers
            | l == Metadata = hs <> toHeaders e
            | otherwise     = hs

    response l s (Encrypted x _ _ _) = response l s x

class Instructions a where
    -- | Determine the bucket and key for the
    -- instructions file, minus the suffix.
    instructions :: a -> (BucketName, ObjectKey)

instance Instructions CreateMultipartUpload where
    instructions = view cmuBucket &&& cmuKey

instance Instructions PutObject where
    instructions = view poBucket &&& poKey

class Instructions a => Encrypt a where
    -- | Create an encryption context.
    encryptWith :: a -> Location -> Envelope -> Encrypted a

instance Encrypt CreateMultipartUpload where
    encryptWith x = Encrypted x []

instance Encrypt PutObject where
    encryptWith x = Encrypted x (len : maybeToList md5)
     where
        len = ("X-Amz-Unencrypted-Content-Length",
            toBS (contentLength (x ^. poBody)))

        md5 = ("X-Amz-Unencrypted-Content-MD5",)
            <$> x ^. poBody . to md5Base64

-- class EncryptPart a b where
--     encryptPart :: Encrypted a -> b -> Encrypted b

-- instance EncryptPart CreateMultipartUpload UploadPart where
--     encryptPart (Encrypted _ _ _ e) x = Encrypted x [] Instructions e

-- decryptUsing :: Decrypt a
--              => Key
--              -> Rs GetInstructions
--              -> a
--              -> m a


-- data PutInstructions = PutInstructions PutObject Text

-- putInstructions :: BucketName -> ObjectKey -> Envelope -> PutInstructions
-- putInstructions = undefined

-- instance AWSRequest PutInstructions where
--     type Rs PutInstructions = PutObjectResponse

-- data GetInstructions = GetInstructions GetObject Text

-- getInstructions :: BucketName -> ObjectKey -> GetInstructions
-- getInstructions = undefined

-- instance AWSRequest GetInstructions where
--     type Rs GetInstructions = Envelope
