{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Sum where

import           Network.AWS.Prelude
import           Network.AWS.S3.Internal

data BucketCannedACL
    = BAuthenticatedRead
    | BPrivate
    | BPublicRead
    | BPublicReadWrite
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BucketCannedACL where
    parser = takeLowerText >>= \case
        "authenticated-read" -> pure BAuthenticatedRead
        "private" -> pure BPrivate
        "public-read" -> pure BPublicRead
        "public-read-write" -> pure BPublicReadWrite
        e -> fromTextError $ "Failure parsing BucketCannedACL from value: '" <> e
           <> "'. Accepted values: authenticated-read, private, public-read, public-read-write"

instance ToText BucketCannedACL where
    toText = \case
        BAuthenticatedRead -> "authenticated-read"
        BPrivate -> "private"
        BPublicRead -> "public-read"
        BPublicReadWrite -> "public-read-write"

instance Hashable     BucketCannedACL
instance ToByteString BucketCannedACL
instance ToQuery      BucketCannedACL
instance ToHeader     BucketCannedACL

instance ToXML BucketCannedACL where
    toXML = toXMLText

data BucketLogsPermission
    = FullControl
    | Read
    | Write
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BucketLogsPermission where
    parser = takeLowerText >>= \case
        "full_control" -> pure FullControl
        "read" -> pure Read
        "write" -> pure Write
        e -> fromTextError $ "Failure parsing BucketLogsPermission from value: '" <> e
           <> "'. Accepted values: FULL_CONTROL, READ, WRITE"

instance ToText BucketLogsPermission where
    toText = \case
        FullControl -> "FULL_CONTROL"
        Read -> "READ"
        Write -> "WRITE"

instance Hashable     BucketLogsPermission
instance ToByteString BucketLogsPermission
instance ToQuery      BucketLogsPermission
instance ToHeader     BucketLogsPermission

instance FromXML BucketLogsPermission where
    parseXML = parseXMLText "BucketLogsPermission"

instance ToXML BucketLogsPermission where
    toXML = toXMLText

data BucketVersioningStatus
    = BVSEnabled
    | BVSSuspended
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BucketVersioningStatus where
    parser = takeLowerText >>= \case
        "enabled" -> pure BVSEnabled
        "suspended" -> pure BVSSuspended
        e -> fromTextError $ "Failure parsing BucketVersioningStatus from value: '" <> e
           <> "'. Accepted values: Enabled, Suspended"

instance ToText BucketVersioningStatus where
    toText = \case
        BVSEnabled -> "Enabled"
        BVSSuspended -> "Suspended"

instance Hashable     BucketVersioningStatus
instance ToByteString BucketVersioningStatus
instance ToQuery      BucketVersioningStatus
instance ToHeader     BucketVersioningStatus

instance FromXML BucketVersioningStatus where
    parseXML = parseXMLText "BucketVersioningStatus"

instance ToXML BucketVersioningStatus where
    toXML = toXMLText

-- | Requests Amazon S3 to encode the object keys in the response and
-- specifies the encoding method to use. An object key may contain any
-- Unicode character; however, XML 1.0 parser cannot parse some characters,
-- such as characters with an ASCII value from 0 to 10. For characters that
-- are not supported in XML 1.0, you can add this parameter to request that
-- Amazon S3 encode the keys in the response.
data EncodingType =
    URL
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EncodingType where
    parser = takeLowerText >>= \case
        "url" -> pure URL
        e -> fromTextError $ "Failure parsing EncodingType from value: '" <> e
           <> "'. Accepted values: url"

instance ToText EncodingType where
    toText = \case
        URL -> "url"

instance Hashable     EncodingType
instance ToByteString EncodingType
instance ToQuery      EncodingType
instance ToHeader     EncodingType

instance FromXML EncodingType where
    parseXML = parseXMLText "EncodingType"

instance ToXML EncodingType where
    toXML = toXMLText

-- | Bucket event for which to send notifications.
data Event
    = S3ObjectCreated
    | S3ObjectCreatedCompleteMultipartUpload
    | S3ObjectCreatedCopy
    | S3ObjectCreatedPost
    | S3ObjectCreatedPut
    | S3ObjectRemoved
    | S3ObjectRemovedDelete
    | S3ObjectRemovedDeleteMarkerCreated
    | S3ReducedRedundancyLostObject
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Event where
    parser = takeLowerText >>= \case
        "s3:objectcreated:*" -> pure S3ObjectCreated
        "s3:objectcreated:completemultipartupload" -> pure S3ObjectCreatedCompleteMultipartUpload
        "s3:objectcreated:copy" -> pure S3ObjectCreatedCopy
        "s3:objectcreated:post" -> pure S3ObjectCreatedPost
        "s3:objectcreated:put" -> pure S3ObjectCreatedPut
        "s3:objectremoved:*" -> pure S3ObjectRemoved
        "s3:objectremoved:delete" -> pure S3ObjectRemovedDelete
        "s3:objectremoved:deletemarkercreated" -> pure S3ObjectRemovedDeleteMarkerCreated
        "s3:reducedredundancylostobject" -> pure S3ReducedRedundancyLostObject
        e -> fromTextError $ "Failure parsing Event from value: '" <> e
           <> "'. Accepted values: s3:ObjectCreated:*, s3:ObjectCreated:CompleteMultipartUpload, s3:ObjectCreated:Copy, s3:ObjectCreated:Post, s3:ObjectCreated:Put, s3:ObjectRemoved:*, s3:ObjectRemoved:Delete, s3:ObjectRemoved:DeleteMarkerCreated, s3:ReducedRedundancyLostObject"

instance ToText Event where
    toText = \case
        S3ObjectCreated -> "s3:ObjectCreated:*"
        S3ObjectCreatedCompleteMultipartUpload -> "s3:ObjectCreated:CompleteMultipartUpload"
        S3ObjectCreatedCopy -> "s3:ObjectCreated:Copy"
        S3ObjectCreatedPost -> "s3:ObjectCreated:Post"
        S3ObjectCreatedPut -> "s3:ObjectCreated:Put"
        S3ObjectRemoved -> "s3:ObjectRemoved:*"
        S3ObjectRemovedDelete -> "s3:ObjectRemoved:Delete"
        S3ObjectRemovedDeleteMarkerCreated -> "s3:ObjectRemoved:DeleteMarkerCreated"
        S3ReducedRedundancyLostObject -> "s3:ReducedRedundancyLostObject"

instance Hashable     Event
instance ToByteString Event
instance ToQuery      Event
instance ToHeader     Event

instance FromXML Event where
    parseXML = parseXMLText "Event"

instance ToXML Event where
    toXML = toXMLText

data ExpirationStatus
    = ESDisabled
    | ESEnabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ExpirationStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure ESDisabled
        "enabled" -> pure ESEnabled
        e -> fromTextError $ "Failure parsing ExpirationStatus from value: '" <> e
           <> "'. Accepted values: Disabled, Enabled"

instance ToText ExpirationStatus where
    toText = \case
        ESDisabled -> "Disabled"
        ESEnabled -> "Enabled"

instance Hashable     ExpirationStatus
instance ToByteString ExpirationStatus
instance ToQuery      ExpirationStatus
instance ToHeader     ExpirationStatus

instance FromXML ExpirationStatus where
    parseXML = parseXMLText "ExpirationStatus"

instance ToXML ExpirationStatus where
    toXML = toXMLText

data FilterRuleName
    = Prefix
    | Suffix
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText FilterRuleName where
    parser = takeLowerText >>= \case
        "prefix" -> pure Prefix
        "suffix" -> pure Suffix
        e -> fromTextError $ "Failure parsing FilterRuleName from value: '" <> e
           <> "'. Accepted values: prefix, suffix"

instance ToText FilterRuleName where
    toText = \case
        Prefix -> "prefix"
        Suffix -> "suffix"

instance Hashable     FilterRuleName
instance ToByteString FilterRuleName
instance ToQuery      FilterRuleName
instance ToHeader     FilterRuleName

instance FromXML FilterRuleName where
    parseXML = parseXMLText "FilterRuleName"

instance ToXML FilterRuleName where
    toXML = toXMLText

data MFADelete
    = MDDisabled
    | MDEnabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MFADelete where
    parser = takeLowerText >>= \case
        "disabled" -> pure MDDisabled
        "enabled" -> pure MDEnabled
        e -> fromTextError $ "Failure parsing MFADelete from value: '" <> e
           <> "'. Accepted values: Disabled, Enabled"

instance ToText MFADelete where
    toText = \case
        MDDisabled -> "Disabled"
        MDEnabled -> "Enabled"

instance Hashable     MFADelete
instance ToByteString MFADelete
instance ToQuery      MFADelete
instance ToHeader     MFADelete

instance ToXML MFADelete where
    toXML = toXMLText

data MFADeleteStatus
    = MDSDisabled
    | MDSEnabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MFADeleteStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure MDSDisabled
        "enabled" -> pure MDSEnabled
        e -> fromTextError $ "Failure parsing MFADeleteStatus from value: '" <> e
           <> "'. Accepted values: Disabled, Enabled"

instance ToText MFADeleteStatus where
    toText = \case
        MDSDisabled -> "Disabled"
        MDSEnabled -> "Enabled"

instance Hashable     MFADeleteStatus
instance ToByteString MFADeleteStatus
instance ToQuery      MFADeleteStatus
instance ToHeader     MFADeleteStatus

instance FromXML MFADeleteStatus where
    parseXML = parseXMLText "MFADeleteStatus"

data MetadataDirective
    = Copy
    | Replace
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MetadataDirective where
    parser = takeLowerText >>= \case
        "copy" -> pure Copy
        "replace" -> pure Replace
        e -> fromTextError $ "Failure parsing MetadataDirective from value: '" <> e
           <> "'. Accepted values: COPY, REPLACE"

instance ToText MetadataDirective where
    toText = \case
        Copy -> "COPY"
        Replace -> "REPLACE"

instance Hashable     MetadataDirective
instance ToByteString MetadataDirective
instance ToQuery      MetadataDirective
instance ToHeader     MetadataDirective

instance ToXML MetadataDirective where
    toXML = toXMLText

data ObjectCannedACL
    = OAuthenticatedRead
    | OBucketOwnerFullControl
    | OBucketOwnerRead
    | OPrivate
    | OPublicRead
    | OPublicReadWrite
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ObjectCannedACL where
    parser = takeLowerText >>= \case
        "authenticated-read" -> pure OAuthenticatedRead
        "bucket-owner-full-control" -> pure OBucketOwnerFullControl
        "bucket-owner-read" -> pure OBucketOwnerRead
        "private" -> pure OPrivate
        "public-read" -> pure OPublicRead
        "public-read-write" -> pure OPublicReadWrite
        e -> fromTextError $ "Failure parsing ObjectCannedACL from value: '" <> e
           <> "'. Accepted values: authenticated-read, bucket-owner-full-control, bucket-owner-read, private, public-read, public-read-write"

instance ToText ObjectCannedACL where
    toText = \case
        OAuthenticatedRead -> "authenticated-read"
        OBucketOwnerFullControl -> "bucket-owner-full-control"
        OBucketOwnerRead -> "bucket-owner-read"
        OPrivate -> "private"
        OPublicRead -> "public-read"
        OPublicReadWrite -> "public-read-write"

instance Hashable     ObjectCannedACL
instance ToByteString ObjectCannedACL
instance ToQuery      ObjectCannedACL
instance ToHeader     ObjectCannedACL

instance ToXML ObjectCannedACL where
    toXML = toXMLText

data ObjectStorageClass
    = OSCGlacier
    | OSCReducedRedundancy
    | OSCStandard
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ObjectStorageClass where
    parser = takeLowerText >>= \case
        "glacier" -> pure OSCGlacier
        "reduced_redundancy" -> pure OSCReducedRedundancy
        "standard" -> pure OSCStandard
        e -> fromTextError $ "Failure parsing ObjectStorageClass from value: '" <> e
           <> "'. Accepted values: GLACIER, REDUCED_REDUNDANCY, STANDARD"

instance ToText ObjectStorageClass where
    toText = \case
        OSCGlacier -> "GLACIER"
        OSCReducedRedundancy -> "REDUCED_REDUNDANCY"
        OSCStandard -> "STANDARD"

instance Hashable     ObjectStorageClass
instance ToByteString ObjectStorageClass
instance ToQuery      ObjectStorageClass
instance ToHeader     ObjectStorageClass

instance FromXML ObjectStorageClass where
    parseXML = parseXMLText "ObjectStorageClass"

data ObjectVersionStorageClass =
    OVSCStandard
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ObjectVersionStorageClass where
    parser = takeLowerText >>= \case
        "standard" -> pure OVSCStandard
        e -> fromTextError $ "Failure parsing ObjectVersionStorageClass from value: '" <> e
           <> "'. Accepted values: STANDARD"

instance ToText ObjectVersionStorageClass where
    toText = \case
        OVSCStandard -> "STANDARD"

instance Hashable     ObjectVersionStorageClass
instance ToByteString ObjectVersionStorageClass
instance ToQuery      ObjectVersionStorageClass
instance ToHeader     ObjectVersionStorageClass

instance FromXML ObjectVersionStorageClass where
    parseXML = parseXMLText "ObjectVersionStorageClass"

data Payer
    = BucketOwner
    | Requester
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Payer where
    parser = takeLowerText >>= \case
        "bucketowner" -> pure BucketOwner
        "requester" -> pure Requester
        e -> fromTextError $ "Failure parsing Payer from value: '" <> e
           <> "'. Accepted values: BucketOwner, Requester"

instance ToText Payer where
    toText = \case
        BucketOwner -> "BucketOwner"
        Requester -> "Requester"

instance Hashable     Payer
instance ToByteString Payer
instance ToQuery      Payer
instance ToHeader     Payer

instance FromXML Payer where
    parseXML = parseXMLText "Payer"

instance ToXML Payer where
    toXML = toXMLText

data Permission
    = PFullControl
    | PRead
    | PReadAcp
    | PWrite
    | PWriteAcp
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Permission where
    parser = takeLowerText >>= \case
        "full_control" -> pure PFullControl
        "read" -> pure PRead
        "read_acp" -> pure PReadAcp
        "write" -> pure PWrite
        "write_acp" -> pure PWriteAcp
        e -> fromTextError $ "Failure parsing Permission from value: '" <> e
           <> "'. Accepted values: FULL_CONTROL, READ, READ_ACP, WRITE, WRITE_ACP"

instance ToText Permission where
    toText = \case
        PFullControl -> "FULL_CONTROL"
        PRead -> "READ"
        PReadAcp -> "READ_ACP"
        PWrite -> "WRITE"
        PWriteAcp -> "WRITE_ACP"

instance Hashable     Permission
instance ToByteString Permission
instance ToQuery      Permission
instance ToHeader     Permission

instance FromXML Permission where
    parseXML = parseXMLText "Permission"

instance ToXML Permission where
    toXML = toXMLText

data Protocol
    = HTTP
    | HTTPS
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Protocol where
    parser = takeLowerText >>= \case
        "http" -> pure HTTP
        "https" -> pure HTTPS
        e -> fromTextError $ "Failure parsing Protocol from value: '" <> e
           <> "'. Accepted values: http, https"

instance ToText Protocol where
    toText = \case
        HTTP -> "http"
        HTTPS -> "https"

instance Hashable     Protocol
instance ToByteString Protocol
instance ToQuery      Protocol
instance ToHeader     Protocol

instance FromXML Protocol where
    parseXML = parseXMLText "Protocol"

instance ToXML Protocol where
    toXML = toXMLText

data ReplicationRuleStatus
    = Disabled
    | Enabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReplicationRuleStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing ReplicationRuleStatus from value: '" <> e
           <> "'. Accepted values: Disabled, Enabled"

instance ToText ReplicationRuleStatus where
    toText = \case
        Disabled -> "Disabled"
        Enabled -> "Enabled"

instance Hashable     ReplicationRuleStatus
instance ToByteString ReplicationRuleStatus
instance ToQuery      ReplicationRuleStatus
instance ToHeader     ReplicationRuleStatus

instance FromXML ReplicationRuleStatus where
    parseXML = parseXMLText "ReplicationRuleStatus"

instance ToXML ReplicationRuleStatus where
    toXML = toXMLText

data ReplicationStatus
    = Complete
    | Failed
    | Pending
    | Replica
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ReplicationStatus where
    parser = takeLowerText >>= \case
        "complete" -> pure Complete
        "failed" -> pure Failed
        "pending" -> pure Pending
        "replica" -> pure Replica
        e -> fromTextError $ "Failure parsing ReplicationStatus from value: '" <> e
           <> "'. Accepted values: COMPLETE, FAILED, PENDING, REPLICA"

instance ToText ReplicationStatus where
    toText = \case
        Complete -> "COMPLETE"
        Failed -> "FAILED"
        Pending -> "PENDING"
        Replica -> "REPLICA"

instance Hashable     ReplicationStatus
instance ToByteString ReplicationStatus
instance ToQuery      ReplicationStatus
instance ToHeader     ReplicationStatus

instance FromXML ReplicationStatus where
    parseXML = parseXMLText "ReplicationStatus"

-- | If present, indicates that the requester was successfully charged for
-- the request.
data RequestCharged =
    RCRequester
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RequestCharged where
    parser = takeLowerText >>= \case
        "requester" -> pure RCRequester
        e -> fromTextError $ "Failure parsing RequestCharged from value: '" <> e
           <> "'. Accepted values: requester"

instance ToText RequestCharged where
    toText = \case
        RCRequester -> "requester"

instance Hashable     RequestCharged
instance ToByteString RequestCharged
instance ToQuery      RequestCharged
instance ToHeader     RequestCharged

instance FromXML RequestCharged where
    parseXML = parseXMLText "RequestCharged"

-- | Confirms that the requester knows that she or he will be charged for the
-- request. Bucket owners need not specify this parameter in their
-- requests. Documentation on downloading objects from requester pays
-- buckets can be found at
-- http:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/ObjectsinRequesterPaysBuckets.html
data RequestPayer =
    RPRequester
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RequestPayer where
    parser = takeLowerText >>= \case
        "requester" -> pure RPRequester
        e -> fromTextError $ "Failure parsing RequestPayer from value: '" <> e
           <> "'. Accepted values: requester"

instance ToText RequestPayer where
    toText = \case
        RPRequester -> "requester"

instance Hashable     RequestPayer
instance ToByteString RequestPayer
instance ToQuery      RequestPayer
instance ToHeader     RequestPayer

instance ToXML RequestPayer where
    toXML = toXMLText

data ServerSideEncryption
    = AES256
    | AWSKMS
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ServerSideEncryption where
    parser = takeLowerText >>= \case
        "aes256" -> pure AES256
        "aws:kms" -> pure AWSKMS
        e -> fromTextError $ "Failure parsing ServerSideEncryption from value: '" <> e
           <> "'. Accepted values: AES256, aws:kms"

instance ToText ServerSideEncryption where
    toText = \case
        AES256 -> "AES256"
        AWSKMS -> "aws:kms"

instance Hashable     ServerSideEncryption
instance ToByteString ServerSideEncryption
instance ToQuery      ServerSideEncryption
instance ToHeader     ServerSideEncryption

instance FromXML ServerSideEncryption where
    parseXML = parseXMLText "ServerSideEncryption"

instance ToXML ServerSideEncryption where
    toXML = toXMLText

data StorageClass
    = ReducedRedundancy
    | Standard
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StorageClass where
    parser = takeLowerText >>= \case
        "reduced_redundancy" -> pure ReducedRedundancy
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing StorageClass from value: '" <> e
           <> "'. Accepted values: REDUCED_REDUNDANCY, STANDARD"

instance ToText StorageClass where
    toText = \case
        ReducedRedundancy -> "REDUCED_REDUNDANCY"
        Standard -> "STANDARD"

instance Hashable     StorageClass
instance ToByteString StorageClass
instance ToQuery      StorageClass
instance ToHeader     StorageClass

instance FromXML StorageClass where
    parseXML = parseXMLText "StorageClass"

instance ToXML StorageClass where
    toXML = toXMLText

data TransitionStorageClass =
    Glacier
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TransitionStorageClass where
    parser = takeLowerText >>= \case
        "glacier" -> pure Glacier
        e -> fromTextError $ "Failure parsing TransitionStorageClass from value: '" <> e
           <> "'. Accepted values: GLACIER"

instance ToText TransitionStorageClass where
    toText = \case
        Glacier -> "GLACIER"

instance Hashable     TransitionStorageClass
instance ToByteString TransitionStorageClass
instance ToQuery      TransitionStorageClass
instance ToHeader     TransitionStorageClass

instance FromXML TransitionStorageClass where
    parseXML = parseXMLText "TransitionStorageClass"

instance ToXML TransitionStorageClass where
    toXML = toXMLText

data Type
    = AmazonCustomerByEmail
    | CanonicalUser
    | Group
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Type where
    parser = takeLowerText >>= \case
        "amazoncustomerbyemail" -> pure AmazonCustomerByEmail
        "canonicaluser" -> pure CanonicalUser
        "group" -> pure Group
        e -> fromTextError $ "Failure parsing Type from value: '" <> e
           <> "'. Accepted values: AmazonCustomerByEmail, CanonicalUser, Group"

instance ToText Type where
    toText = \case
        AmazonCustomerByEmail -> "AmazonCustomerByEmail"
        CanonicalUser -> "CanonicalUser"
        Group -> "Group"

instance Hashable     Type
instance ToByteString Type
instance ToQuery      Type
instance ToHeader     Type

instance FromXML Type where
    parseXML = parseXMLText "Type"

instance ToXML Type where
    toXML = toXMLText
