{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Relational Database Service
--
-- Amazon Relational Database Service (Amazon RDS) is a web service that
-- makes it easier to set up, operate, and scale a relational database in
-- the cloud. It provides cost-efficient, resizeable capacity for an
-- industry-standard relational database and manages common database
-- administration tasks, freeing up developers to focus on what makes their
-- applications and businesses unique.
--
-- Amazon RDS gives you access to the capabilities of a MySQL, PostgreSQL,
-- Microsoft SQL Server, Oracle, or Aurora database server. This means the
-- code, applications, and tools you already use today with your existing
-- databases work with Amazon RDS without modification. Amazon RDS
-- automatically backs up your database and maintains the database software
-- that powers your DB instance. Amazon RDS is flexible: you can scale your
-- database instance\'s compute resources and storage capacity to meet your
-- application\'s demand. As with all Amazon Web Services, there are no
-- up-front investments, and you pay only for the resources you use.
--
-- This is an interface reference for Amazon RDS. It contains documentation
-- for a programming or command line interface you can use to manage Amazon
-- RDS. Note that Amazon RDS is asynchronous, which means that some
-- interfaces might require techniques such as polling or callback
-- functions to determine when a command has been applied. In this
-- reference, the parameter descriptions indicate whether a command is
-- applied immediately, on the next instance reboot, or during the
-- maintenance window. For a summary of the Amazon RDS interfaces, go to
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Welcome.html#Welcome.Interfaces Available RDS Interfaces>.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.RDS
    (
    -- * Service Configuration
      rDS

    -- * Errors
    -- $errors

    -- ** PointInTimeRestoreNotEnabledFault
    , _PointInTimeRestoreNotEnabledFault

    -- ** InvalidDBParameterGroupStateFault
    , _InvalidDBParameterGroupStateFault

    -- ** ReservedDBInstanceQuotaExceededFault
    , _ReservedDBInstanceQuotaExceededFault

    -- ** SourceNotFoundFault
    , _SourceNotFoundFault

    -- ** CertificateNotFoundFault
    , _CertificateNotFoundFault

    -- ** AuthorizationQuotaExceededFault
    , _AuthorizationQuotaExceededFault

    -- ** DBClusterSnapshotAlreadyExistsFault
    , _DBClusterSnapshotAlreadyExistsFault

    -- ** DBParameterGroupAlreadyExistsFault
    , _DBParameterGroupAlreadyExistsFault

    -- ** DBParameterGroupQuotaExceededFault
    , _DBParameterGroupQuotaExceededFault

    -- ** InsufficientDBClusterCapacityFault
    , _InsufficientDBClusterCapacityFault

    -- ** ReservedDBInstanceAlreadyExistsFault
    , _ReservedDBInstanceAlreadyExistsFault

    -- ** ProvisionedIOPSNotAvailableInAZFault
    , _ProvisionedIOPSNotAvailableInAZFault

    -- ** AuthorizationAlreadyExistsFault
    , _AuthorizationAlreadyExistsFault

    -- ** SubscriptionCategoryNotFoundFault
    , _SubscriptionCategoryNotFoundFault

    -- ** SubscriptionNotFoundFault
    , _SubscriptionNotFoundFault

    -- ** InvalidSubnet
    , _InvalidSubnet

    -- ** DBSubnetQuotaExceededFault
    , _DBSubnetQuotaExceededFault

    -- ** OptionGroupNotFoundFault
    , _OptionGroupNotFoundFault

    -- ** DBClusterNotFoundFault
    , _DBClusterNotFoundFault

    -- ** DBLogFileNotFoundFault
    , _DBLogFileNotFoundFault

    -- ** DBClusterAlreadyExistsFault
    , _DBClusterAlreadyExistsFault

    -- ** StorageTypeNotSupportedFault
    , _StorageTypeNotSupportedFault

    -- ** DBSecurityGroupQuotaExceededFault
    , _DBSecurityGroupQuotaExceededFault

    -- ** OptionGroupAlreadyExistsFault
    , _OptionGroupAlreadyExistsFault

    -- ** OptionGroupQuotaExceededFault
    , _OptionGroupQuotaExceededFault

    -- ** DBSecurityGroupAlreadyExistsFault
    , _DBSecurityGroupAlreadyExistsFault

    -- ** SNSTopicARNNotFoundFault
    , _SNSTopicARNNotFoundFault

    -- ** InvalidEventSubscriptionStateFault
    , _InvalidEventSubscriptionStateFault

    -- ** KMSKeyNotAccessibleFault
    , _KMSKeyNotAccessibleFault

    -- ** DBSnapshotNotFoundFault
    , _DBSnapshotNotFoundFault

    -- ** DBClusterParameterGroupNotFoundFault
    , _DBClusterParameterGroupNotFoundFault

    -- ** DBClusterQuotaExceededFault
    , _DBClusterQuotaExceededFault

    -- ** SnapshotQuotaExceededFault
    , _SnapshotQuotaExceededFault

    -- ** DBSubnetGroupAlreadyExistsFault
    , _DBSubnetGroupAlreadyExistsFault

    -- ** SNSNoAuthorizationFault
    , _SNSNoAuthorizationFault

    -- ** DBSecurityGroupNotFoundFault
    , _DBSecurityGroupNotFoundFault

    -- ** DBSecurityGroupNotSupportedFault
    , _DBSecurityGroupNotSupportedFault

    -- ** InstanceQuotaExceededFault
    , _InstanceQuotaExceededFault

    -- ** DomainNotFoundFault
    , _DomainNotFoundFault

    -- ** DBParameterGroupNotFoundFault
    , _DBParameterGroupNotFoundFault

    -- ** InvalidDBSubnetGroupFault
    , _InvalidDBSubnetGroupFault

    -- ** ReservedDBInstancesOfferingNotFoundFault
    , _ReservedDBInstancesOfferingNotFoundFault

    -- ** InvalidDBSubnetStateFault
    , _InvalidDBSubnetStateFault

    -- ** DBClusterSnapshotNotFoundFault
    , _DBClusterSnapshotNotFoundFault

    -- ** SNSInvalidTopicFault
    , _SNSInvalidTopicFault

    -- ** InsufficientDBInstanceCapacityFault
    , _InsufficientDBInstanceCapacityFault

    -- ** InvalidDBClusterSnapshotStateFault
    , _InvalidDBClusterSnapshotStateFault

    -- ** SubscriptionAlreadyExistFault
    , _SubscriptionAlreadyExistFault

    -- ** InvalidVPCNetworkStateFault
    , _InvalidVPCNetworkStateFault

    -- ** AuthorizationNotFoundFault
    , _AuthorizationNotFoundFault

    -- ** ReservedDBInstanceNotFoundFault
    , _ReservedDBInstanceNotFoundFault

    -- ** DBSubnetGroupQuotaExceededFault
    , _DBSubnetGroupQuotaExceededFault

    -- ** DBSubnetGroupNotAllowedFault
    , _DBSubnetGroupNotAllowedFault

    -- ** EventSubscriptionQuotaExceededFault
    , _EventSubscriptionQuotaExceededFault

    -- ** InsufficientStorageClusterCapacityFault
    , _InsufficientStorageClusterCapacityFault

    -- ** InvalidOptionGroupStateFault
    , _InvalidOptionGroupStateFault

    -- ** InvalidDBClusterStateFault
    , _InvalidDBClusterStateFault

    -- ** DBInstanceAlreadyExistsFault
    , _DBInstanceAlreadyExistsFault

    -- ** InvalidRestoreFault
    , _InvalidRestoreFault

    -- ** InvalidDBSecurityGroupStateFault
    , _InvalidDBSecurityGroupStateFault

    -- ** ResourceNotFoundFault
    , _ResourceNotFoundFault

    -- ** InsufficientDomainCapacityFault
    , _InsufficientDomainCapacityFault

    -- ** DBSubnetGroupNotFoundFault
    , _DBSubnetGroupNotFoundFault

    -- ** DBUpgradeDependencyFailureFault
    , _DBUpgradeDependencyFailureFault

    -- ** InvalidDBInstanceStateFault
    , _InvalidDBInstanceStateFault

    -- ** DBSnapshotAlreadyExistsFault
    , _DBSnapshotAlreadyExistsFault

    -- ** DBInstanceNotFoundFault
    , _DBInstanceNotFoundFault

    -- ** StorageQuotaExceededFault
    , _StorageQuotaExceededFault

    -- ** InvalidDBSnapshotStateFault
    , _InvalidDBSnapshotStateFault

    -- ** InvalidDBSubnetGroupStateFault
    , _InvalidDBSubnetGroupStateFault

    -- ** DBSubnetGroupDoesNotCoverEnoughAZs
    , _DBSubnetGroupDoesNotCoverEnoughAZs

    -- ** SubnetAlreadyInUse
    , _SubnetAlreadyInUse

    -- * Waiters
    -- $waiters

    -- ** DBInstanceAvailable
    , dbInstanceAvailable

    -- ** DBSnapshotCompleted
    , dbSnapshotCompleted

    -- ** DBInstanceDeleted
    , dbInstanceDeleted

    -- * Operations
    -- $operations

    -- ** DescribeDBClusterParameterGroups
    , module Network.AWS.RDS.DescribeDBClusterParameterGroups

    -- ** PromoteReadReplica
    , module Network.AWS.RDS.PromoteReadReplica

    -- ** DescribeDBEngineVersions (Paginated)
    , module Network.AWS.RDS.DescribeDBEngineVersions

    -- ** CopyDBSnapshot
    , module Network.AWS.RDS.CopyDBSnapshot

    -- ** AddSourceIdentifierToSubscription
    , module Network.AWS.RDS.AddSourceIdentifierToSubscription

    -- ** ModifyDBInstance
    , module Network.AWS.RDS.ModifyDBInstance

    -- ** ModifyEventSubscription
    , module Network.AWS.RDS.ModifyEventSubscription

    -- ** ResetDBClusterParameterGroup
    , module Network.AWS.RDS.ResetDBClusterParameterGroup

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.RDS.DescribeEvents

    -- ** DescribeEngineDefaultParameters (Paginated)
    , module Network.AWS.RDS.DescribeEngineDefaultParameters

    -- ** DescribeOptionGroups (Paginated)
    , module Network.AWS.RDS.DescribeOptionGroups

    -- ** DescribeDBLogFiles (Paginated)
    , module Network.AWS.RDS.DescribeDBLogFiles

    -- ** DescribeDBClusters
    , module Network.AWS.RDS.DescribeDBClusters

    -- ** ModifyDBSubnetGroup
    , module Network.AWS.RDS.ModifyDBSubnetGroup

    -- ** ListTagsForResource
    , module Network.AWS.RDS.ListTagsForResource

    -- ** DeleteOptionGroup
    , module Network.AWS.RDS.DeleteOptionGroup

    -- ** DeleteDBCluster
    , module Network.AWS.RDS.DeleteDBCluster

    -- ** DescribeReservedDBInstances (Paginated)
    , module Network.AWS.RDS.DescribeReservedDBInstances

    -- ** CopyDBParameterGroup
    , module Network.AWS.RDS.CopyDBParameterGroup

    -- ** RemoveSourceIdentifierFromSubscription
    , module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription

    -- ** DescribeEngineDefaultClusterParameters
    , module Network.AWS.RDS.DescribeEngineDefaultClusterParameters

    -- ** RemoveTagsFromResource
    , module Network.AWS.RDS.RemoveTagsFromResource

    -- ** RestoreDBInstanceFromDBSnapshot
    , module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot

    -- ** CreateEventSubscription
    , module Network.AWS.RDS.CreateEventSubscription

    -- ** PurchaseReservedDBInstancesOffering
    , module Network.AWS.RDS.PurchaseReservedDBInstancesOffering

    -- ** CreateDBInstance
    , module Network.AWS.RDS.CreateDBInstance

    -- ** DeleteDBClusterParameterGroup
    , module Network.AWS.RDS.DeleteDBClusterParameterGroup

    -- ** DescribeCertificates
    , module Network.AWS.RDS.DescribeCertificates

    -- ** AuthorizeDBSecurityGroupIngress
    , module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress

    -- ** RestoreDBClusterFromSnapshot
    , module Network.AWS.RDS.RestoreDBClusterFromSnapshot

    -- ** DescribeOrderableDBInstanceOptions (Paginated)
    , module Network.AWS.RDS.DescribeOrderableDBInstanceOptions

    -- ** CreateDBClusterParameterGroup
    , module Network.AWS.RDS.CreateDBClusterParameterGroup

    -- ** CreateDBSnapshot
    , module Network.AWS.RDS.CreateDBSnapshot

    -- ** DeleteEventSubscription
    , module Network.AWS.RDS.DeleteEventSubscription

    -- ** DescribeDBParameterGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBParameterGroups

    -- ** DeleteDBClusterSnapshot
    , module Network.AWS.RDS.DeleteDBClusterSnapshot

    -- ** DescribeOptionGroupOptions (Paginated)
    , module Network.AWS.RDS.DescribeOptionGroupOptions

    -- ** DescribeEventSubscriptions (Paginated)
    , module Network.AWS.RDS.DescribeEventSubscriptions

    -- ** AddTagsToResource
    , module Network.AWS.RDS.AddTagsToResource

    -- ** DescribeDBParameters (Paginated)
    , module Network.AWS.RDS.DescribeDBParameters

    -- ** CreateDBClusterSnapshot
    , module Network.AWS.RDS.CreateDBClusterSnapshot

    -- ** DescribeDBSnapshots (Paginated)
    , module Network.AWS.RDS.DescribeDBSnapshots

    -- ** DescribeDBSubnetGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBSubnetGroups

    -- ** ModifyOptionGroup
    , module Network.AWS.RDS.ModifyOptionGroup

    -- ** CreateDBParameterGroup
    , module Network.AWS.RDS.CreateDBParameterGroup

    -- ** ModifyDBCluster
    , module Network.AWS.RDS.ModifyDBCluster

    -- ** DescribeEventCategories
    , module Network.AWS.RDS.DescribeEventCategories

    -- ** ModifyDBClusterParameterGroup
    , module Network.AWS.RDS.ModifyDBClusterParameterGroup

    -- ** RestoreDBInstanceToPointInTime
    , module Network.AWS.RDS.RestoreDBInstanceToPointInTime

    -- ** DescribePendingMaintenanceActions
    , module Network.AWS.RDS.DescribePendingMaintenanceActions

    -- ** CopyDBClusterSnapshot
    , module Network.AWS.RDS.CopyDBClusterSnapshot

    -- ** ResetDBParameterGroup
    , module Network.AWS.RDS.ResetDBParameterGroup

    -- ** CreateDBCluster
    , module Network.AWS.RDS.CreateDBCluster

    -- ** FailoverDBCluster
    , module Network.AWS.RDS.FailoverDBCluster

    -- ** RevokeDBSecurityGroupIngress
    , module Network.AWS.RDS.RevokeDBSecurityGroupIngress

    -- ** ModifyDBParameterGroup
    , module Network.AWS.RDS.ModifyDBParameterGroup

    -- ** ApplyPendingMaintenanceAction
    , module Network.AWS.RDS.ApplyPendingMaintenanceAction

    -- ** CreateOptionGroup
    , module Network.AWS.RDS.CreateOptionGroup

    -- ** DescribeAccountAttributes
    , module Network.AWS.RDS.DescribeAccountAttributes

    -- ** DeleteDBSnapshot
    , module Network.AWS.RDS.DeleteDBSnapshot

    -- ** DescribeDBClusterParameters
    , module Network.AWS.RDS.DescribeDBClusterParameters

    -- ** DeleteDBSubnetGroup
    , module Network.AWS.RDS.DeleteDBSubnetGroup

    -- ** CreateDBSecurityGroup
    , module Network.AWS.RDS.CreateDBSecurityGroup

    -- ** DescribeDBClusterSnapshots
    , module Network.AWS.RDS.DescribeDBClusterSnapshots

    -- ** RebootDBInstance
    , module Network.AWS.RDS.RebootDBInstance

    -- ** CreateDBSubnetGroup
    , module Network.AWS.RDS.CreateDBSubnetGroup

    -- ** DescribeReservedDBInstancesOfferings (Paginated)
    , module Network.AWS.RDS.DescribeReservedDBInstancesOfferings

    -- ** DeleteDBSecurityGroup
    , module Network.AWS.RDS.DeleteDBSecurityGroup

    -- ** DeleteDBInstance
    , module Network.AWS.RDS.DeleteDBInstance

    -- ** CreateDBInstanceReadReplica
    , module Network.AWS.RDS.CreateDBInstanceReadReplica

    -- ** DeleteDBParameterGroup
    , module Network.AWS.RDS.DeleteDBParameterGroup

    -- ** DescribeDBSecurityGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBSecurityGroups

    -- ** CopyOptionGroup
    , module Network.AWS.RDS.CopyOptionGroup

    -- ** RestoreDBClusterToPointInTime
    , module Network.AWS.RDS.RestoreDBClusterToPointInTime

    -- ** DescribeDBInstances (Paginated)
    , module Network.AWS.RDS.DescribeDBInstances

    -- ** DownloadDBLogFilePortion (Paginated)
    , module Network.AWS.RDS.DownloadDBLogFilePortion

    -- * Types

    -- ** ApplyMethod
    , ApplyMethod (..)

    -- ** SourceType
    , SourceType (..)

    -- ** AccountQuota
    , AccountQuota
    , accountQuota
    , aqMax
    , aqUsed
    , aqAccountQuotaName

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- ** Certificate
    , Certificate
    , certificate
    , cCertificateType
    , cValidTill
    , cCertificateIdentifier
    , cThumbprint
    , cValidFrom

    -- ** CharacterSet
    , CharacterSet
    , characterSet
    , csCharacterSetName
    , csCharacterSetDescription

    -- ** DBCluster
    , DBCluster
    , dbCluster
    , dcEngineVersion
    , dcStatus
    , dcDBClusterIdentifier
    , dcDBClusterMembers
    , dcDBClusterParameterGroup
    , dcMasterUsername
    , dcEarliestRestorableTime
    , dcEngine
    , dcLatestRestorableTime
    , dcPreferredMaintenanceWindow
    , dcAvailabilityZones
    , dcCharacterSetName
    , dcPreferredBackupWindow
    , dcVPCSecurityGroups
    , dcBackupRetentionPeriod
    , dcDBSubnetGroup
    , dcDatabaseName
    , dcAllocatedStorage
    , dcEndpoint
    , dcPercentProgress
    , dcPort
    , dcDBClusterOptionGroupMemberships

    -- ** DBClusterMember
    , DBClusterMember
    , dbClusterMember
    , dcmDBInstanceIdentifier
    , dcmIsClusterWriter
    , dcmDBClusterParameterGroupStatus

    -- ** DBClusterOptionGroupStatus
    , DBClusterOptionGroupStatus
    , dbClusterOptionGroupStatus
    , dcogsStatus
    , dcogsDBClusterOptionGroupName

    -- ** DBClusterParameterGroup
    , DBClusterParameterGroup
    , dbClusterParameterGroup
    , dcpgDBParameterGroupFamily
    , dcpgDBClusterParameterGroupName
    , dcpgDescription

    -- ** DBClusterParameterGroupNameMessage
    , DBClusterParameterGroupNameMessage
    , dbClusterParameterGroupNameMessage
    , dcpgnmDBClusterParameterGroupName

    -- ** DBClusterSnapshot
    , DBClusterSnapshot
    , dbClusterSnapshot
    , dcsEngineVersion
    , dcsStatus
    , dcsDBClusterIdentifier
    , dcsMasterUsername
    , dcsVPCId
    , dcsDBClusterSnapshotIdentifier
    , dcsEngine
    , dcsLicenseModel
    , dcsAvailabilityZones
    , dcsSnapshotType
    , dcsSnapshotCreateTime
    , dcsAllocatedStorage
    , dcsClusterCreateTime
    , dcsPercentProgress
    , dcsPort

    -- ** DBEngineVersion
    , DBEngineVersion
    , dbEngineVersion
    , devEngineVersion
    , devDBEngineVersionDescription
    , devDefaultCharacterSet
    , devEngine
    , devDBParameterGroupFamily
    , devSupportedCharacterSets
    , devDBEngineDescription

    -- ** DBInstance
    , DBInstance
    , dbInstance
    , diEngineVersion
    , diDBSecurityGroups
    , diStorageEncrypted
    , diDBClusterIdentifier
    , diPubliclyAccessible
    , diAutoMinorVersionUpgrade
    , diMasterUsername
    , diReadReplicaDBInstanceIdentifiers
    , diIOPS
    , diInstanceCreateTime
    , diReadReplicaSourceDBInstanceIdentifier
    , diEngine
    , diLatestRestorableTime
    , diDBInstanceClass
    , diLicenseModel
    , diPreferredMaintenanceWindow
    , diCACertificateIdentifier
    , diDBInstanceIdentifier
    , diCharacterSetName
    , diKMSKeyId
    , diPreferredBackupWindow
    , diAvailabilityZone
    , diVPCSecurityGroups
    , diBackupRetentionPeriod
    , diDBSubnetGroup
    , diMultiAZ
    , diOptionGroupMemberships
    , diSecondaryAvailabilityZone
    , diAllocatedStorage
    , diDBiResourceId
    , diDBParameterGroups
    , diCopyTagsToSnapshot
    , diTDECredentialARN
    , diEndpoint
    , diDBInstanceStatus
    , diDBInstancePort
    , diPendingModifiedValues
    , diStorageType
    , diStatusInfos
    , diDomainMemberships
    , diDBName

    -- ** DBInstanceStatusInfo
    , DBInstanceStatusInfo
    , dbInstanceStatusInfo
    , disiStatus
    , disiNormal
    , disiStatusType
    , disiMessage

    -- ** DBParameterGroup
    , DBParameterGroup
    , dbParameterGroup
    , dpgDBParameterGroupFamily
    , dpgDBParameterGroupName
    , dpgDescription

    -- ** DBParameterGroupNameMessage
    , DBParameterGroupNameMessage
    , dbParameterGroupNameMessage
    , dpgnmDBParameterGroupName

    -- ** DBParameterGroupStatus
    , DBParameterGroupStatus
    , dbParameterGroupStatus
    , dpgsDBParameterGroupName
    , dpgsParameterApplyStatus

    -- ** DBSecurityGroup
    , DBSecurityGroup
    , dbSecurityGroup
    , dbsgVPCId
    , dbsgOwnerId
    , dbsgIPRanges
    , dbsgDBSecurityGroupName
    , dbsgEC2SecurityGroups
    , dbsgDBSecurityGroupDescription

    -- ** DBSecurityGroupMembership
    , DBSecurityGroupMembership
    , dbSecurityGroupMembership
    , dsgmStatus
    , dsgmDBSecurityGroupName

    -- ** DBSnapshot
    , DBSnapshot
    , dbSnapshot
    , dsEngineVersion
    , dsStatus
    , dsMasterUsername
    , dsSourceRegion
    , dsIOPS
    , dsVPCId
    , dsInstanceCreateTime
    , dsEngine
    , dsEncrypted
    , dsDBSnapshotIdentifier
    , dsLicenseModel
    , dsSourceDBSnapshotIdentifier
    , dsSnapshotType
    , dsDBInstanceIdentifier
    , dsKMSKeyId
    , dsAvailabilityZone
    , dsSnapshotCreateTime
    , dsAllocatedStorage
    , dsOptionGroupName
    , dsTDECredentialARN
    , dsPercentProgress
    , dsPort
    , dsStorageType

    -- ** DBSubnetGroup
    , DBSubnetGroup
    , dbSubnetGroup
    , dsgDBSubnetGroupName
    , dsgVPCId
    , dsgSubnets
    , dsgDBSubnetGroupDescription
    , dsgSubnetGroupStatus

    -- ** DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails
    , describeDBLogFilesDetails
    , ddlfdLastWritten
    , ddlfdSize
    , ddlfdLogFileName

    -- ** DomainMembership
    , DomainMembership
    , domainMembership
    , dmStatus
    , dmDomain
    , dmConnectivity

    -- ** EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , esgStatus
    , esgEC2SecurityGroupOwnerId
    , esgEC2SecurityGroupName
    , esgEC2SecurityGroupId

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- ** EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- ** Event
    , Event
    , event
    , eSourceType
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage

    -- ** EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmSourceType
    , ecmEventCategories

    -- ** EventSubscription
    , EventSubscription
    , eventSubscription
    , esStatus
    , esCustomerAWSId
    , esCustSubscriptionId
    , esSNSTopicARN
    , esEnabled
    , esSourceType
    , esSubscriptionCreationTime
    , esEventCategoriesList
    , esSourceIdsList

    -- ** Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- ** IPRange
    , IPRange
    , ipRange
    , irStatus
    , irCIdRIP

    -- ** Option
    , Option
    , option
    , oOptionName
    , oPermanent
    , oPersistent
    , oOptionDescription
    , oOptionSettings
    , oVPCSecurityGroupMemberships
    , oDBSecurityGroupMemberships
    , oPort

    -- ** OptionConfiguration
    , OptionConfiguration
    , optionConfiguration
    , ocOptionSettings
    , ocVPCSecurityGroupMemberships
    , ocDBSecurityGroupMemberships
    , ocPort
    , ocOptionName

    -- ** OptionGroup
    , OptionGroup
    , optionGroup
    , ogOptionGroupDescription
    , ogVPCId
    , ogAllowsVPCAndNonVPCInstanceMemberships
    , ogEngineName
    , ogMajorEngineVersion
    , ogOptions
    , ogOptionGroupName

    -- ** OptionGroupMembership
    , OptionGroupMembership
    , optionGroupMembership
    , ogmStatus
    , ogmOptionGroupName

    -- ** OptionGroupOption
    , OptionGroupOption
    , optionGroupOption
    , ogoMinimumRequiredMinorEngineVersion
    , ogoPermanent
    , ogoPersistent
    , ogoEngineName
    , ogoMajorEngineVersion
    , ogoName
    , ogoDefaultPort
    , ogoOptionGroupOptionSettings
    , ogoPortRequired
    , ogoDescription
    , ogoOptionsDependedOn

    -- ** OptionGroupOptionSetting
    , OptionGroupOptionSetting
    , optionGroupOptionSetting
    , ogosApplyType
    , ogosSettingName
    , ogosDefaultValue
    , ogosIsModifiable
    , ogosSettingDescription
    , ogosAllowedValues

    -- ** OptionSetting
    , OptionSetting
    , optionSetting
    , osIsCollection
    , osApplyType
    , osValue
    , osName
    , osDefaultValue
    , osIsModifiable
    , osDataType
    , osAllowedValues
    , osDescription

    -- ** OrderableDBInstanceOption
    , OrderableDBInstanceOption
    , orderableDBInstanceOption
    , odioEngineVersion
    , odioMultiAZCapable
    , odioEngine
    , odioSupportsIOPS
    , odioDBInstanceClass
    , odioLicenseModel
    , odioAvailabilityZones
    , odioSupportsStorageEncryption
    , odioReadReplicaCapable
    , odioVPC
    , odioStorageType

    -- ** Parameter
    , Parameter
    , parameter
    , pApplyType
    , pParameterValue
    , pApplyMethod
    , pMinimumEngineVersion
    , pSource
    , pIsModifiable
    , pDataType
    , pAllowedValues
    , pParameterName
    , pDescription

    -- ** PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAutoAppliedAfterDate
    , pmaAction
    , pmaOptInStatus
    , pmaDescription
    , pmaForcedApplyDate
    , pmaCurrentApplyDate

    -- ** PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEngineVersion
    , pmvMasterUserPassword
    , pmvIOPS
    , pmvDBInstanceClass
    , pmvCACertificateIdentifier
    , pmvDBInstanceIdentifier
    , pmvBackupRetentionPeriod
    , pmvMultiAZ
    , pmvAllocatedStorage
    , pmvPort
    , pmvStorageType

    -- ** RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- ** ReservedDBInstance
    , ReservedDBInstance
    , reservedDBInstance
    , rdiDBInstanceCount
    , rdiState
    , rdiCurrencyCode
    , rdiStartTime
    , rdiProductDescription
    , rdiReservedDBInstanceId
    , rdiDBInstanceClass
    , rdiMultiAZ
    , rdiReservedDBInstancesOfferingId
    , rdiRecurringCharges
    , rdiOfferingType
    , rdiUsagePrice
    , rdiFixedPrice
    , rdiDuration

    -- ** ReservedDBInstancesOffering
    , ReservedDBInstancesOffering
    , reservedDBInstancesOffering
    , rdioCurrencyCode
    , rdioProductDescription
    , rdioDBInstanceClass
    , rdioMultiAZ
    , rdioReservedDBInstancesOfferingId
    , rdioRecurringCharges
    , rdioOfferingType
    , rdioUsagePrice
    , rdioFixedPrice
    , rdioDuration

    -- ** ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions
    , resourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- ** Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import           Network.AWS.RDS.AddSourceIdentifierToSubscription
import           Network.AWS.RDS.AddTagsToResource
import           Network.AWS.RDS.ApplyPendingMaintenanceAction
import           Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
import           Network.AWS.RDS.CopyDBClusterSnapshot
import           Network.AWS.RDS.CopyDBParameterGroup
import           Network.AWS.RDS.CopyDBSnapshot
import           Network.AWS.RDS.CopyOptionGroup
import           Network.AWS.RDS.CreateDBCluster
import           Network.AWS.RDS.CreateDBClusterParameterGroup
import           Network.AWS.RDS.CreateDBClusterSnapshot
import           Network.AWS.RDS.CreateDBInstance
import           Network.AWS.RDS.CreateDBInstanceReadReplica
import           Network.AWS.RDS.CreateDBParameterGroup
import           Network.AWS.RDS.CreateDBSecurityGroup
import           Network.AWS.RDS.CreateDBSnapshot
import           Network.AWS.RDS.CreateDBSubnetGroup
import           Network.AWS.RDS.CreateEventSubscription
import           Network.AWS.RDS.CreateOptionGroup
import           Network.AWS.RDS.DeleteDBCluster
import           Network.AWS.RDS.DeleteDBClusterParameterGroup
import           Network.AWS.RDS.DeleteDBClusterSnapshot
import           Network.AWS.RDS.DeleteDBInstance
import           Network.AWS.RDS.DeleteDBParameterGroup
import           Network.AWS.RDS.DeleteDBSecurityGroup
import           Network.AWS.RDS.DeleteDBSnapshot
import           Network.AWS.RDS.DeleteDBSubnetGroup
import           Network.AWS.RDS.DeleteEventSubscription
import           Network.AWS.RDS.DeleteOptionGroup
import           Network.AWS.RDS.DescribeAccountAttributes
import           Network.AWS.RDS.DescribeCertificates
import           Network.AWS.RDS.DescribeDBClusterParameterGroups
import           Network.AWS.RDS.DescribeDBClusterParameters
import           Network.AWS.RDS.DescribeDBClusters
import           Network.AWS.RDS.DescribeDBClusterSnapshots
import           Network.AWS.RDS.DescribeDBEngineVersions
import           Network.AWS.RDS.DescribeDBInstances
import           Network.AWS.RDS.DescribeDBLogFiles
import           Network.AWS.RDS.DescribeDBParameterGroups
import           Network.AWS.RDS.DescribeDBParameters
import           Network.AWS.RDS.DescribeDBSecurityGroups
import           Network.AWS.RDS.DescribeDBSnapshots
import           Network.AWS.RDS.DescribeDBSubnetGroups
import           Network.AWS.RDS.DescribeEngineDefaultClusterParameters
import           Network.AWS.RDS.DescribeEngineDefaultParameters
import           Network.AWS.RDS.DescribeEventCategories
import           Network.AWS.RDS.DescribeEvents
import           Network.AWS.RDS.DescribeEventSubscriptions
import           Network.AWS.RDS.DescribeOptionGroupOptions
import           Network.AWS.RDS.DescribeOptionGroups
import           Network.AWS.RDS.DescribeOrderableDBInstanceOptions
import           Network.AWS.RDS.DescribePendingMaintenanceActions
import           Network.AWS.RDS.DescribeReservedDBInstances
import           Network.AWS.RDS.DescribeReservedDBInstancesOfferings
import           Network.AWS.RDS.DownloadDBLogFilePortion
import           Network.AWS.RDS.FailoverDBCluster
import           Network.AWS.RDS.ListTagsForResource
import           Network.AWS.RDS.ModifyDBCluster
import           Network.AWS.RDS.ModifyDBClusterParameterGroup
import           Network.AWS.RDS.ModifyDBInstance
import           Network.AWS.RDS.ModifyDBParameterGroup
import           Network.AWS.RDS.ModifyDBSubnetGroup
import           Network.AWS.RDS.ModifyEventSubscription
import           Network.AWS.RDS.ModifyOptionGroup
import           Network.AWS.RDS.PromoteReadReplica
import           Network.AWS.RDS.PurchaseReservedDBInstancesOffering
import           Network.AWS.RDS.RebootDBInstance
import           Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
import           Network.AWS.RDS.RemoveTagsFromResource
import           Network.AWS.RDS.ResetDBClusterParameterGroup
import           Network.AWS.RDS.ResetDBParameterGroup
import           Network.AWS.RDS.RestoreDBClusterFromSnapshot
import           Network.AWS.RDS.RestoreDBClusterToPointInTime
import           Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
import           Network.AWS.RDS.RestoreDBInstanceToPointInTime
import           Network.AWS.RDS.RevokeDBSecurityGroupIngress
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'RDS'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
