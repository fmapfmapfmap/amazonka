{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KMS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.KMS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.KMS
import Test.AWS.KMS.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testEncrypt $
--             encrypt
--
--         , testListGrants $
--             listGrants
--
--         , testDisableKeyRotation $
--             disableKeyRotation
--
--         , testGenerateDataKeyWithoutPlaintext $
--             generateDataKeyWithoutPlaintext
--
--         , testEnableKeyRotation $
--             enableKeyRotation
--
--         , testCreateAlias $
--             createAlias
--
--         , testCreateGrant $
--             createGrant
--
--         , testListAliases $
--             listAliases
--
--         , testGenerateRandom $
--             generateRandom
--
--         , testCreateKey $
--             createKey
--
--         , testDisableKey $
--             disableKey
--
--         , testRetireGrant $
--             retireGrant
--
--         , testListKeys $
--             listKeys
--
--         , testGetKeyRotationStatus $
--             getKeyRotationStatus
--
--         , testGenerateDataKey $
--             generateDataKey
--
--         , testDeleteAlias $
--             deleteAlias
--
--         , testUpdateAlias $
--             updateAlias
--
--         , testDescribeKey $
--             describeKey
--
--         , testDecrypt $
--             decrypt
--
--         , testUpdateKeyDescription $
--             updateKeyDescription
--
--         , testReEncrypt $
--             reEncrypt
--
--         , testListKeyPolicies $
--             listKeyPolicies
--
--         , testPutKeyPolicy $
--             putKeyPolicy
--
--         , testEnableKey $
--             enableKey
--
--         , testRevokeGrant $
--             revokeGrant
--
--         , testGetKeyPolicy $
--             getKeyPolicy
--
--           ]

--     , testGroup "response"
--         [ testEncryptResponse $
--             encryptResponse
--
--         , testListGrantsResponse $
--             listGrantsResponse
--
--         , testDisableKeyRotationResponse $
--             disableKeyRotationResponse
--
--         , testGenerateDataKeyWithoutPlaintextResponse $
--             generateDataKeyWithoutPlaintextResponse
--
--         , testEnableKeyRotationResponse $
--             enableKeyRotationResponse
--
--         , testCreateAliasResponse $
--             createAliasResponse
--
--         , testCreateGrantResponse $
--             createGrantResponse
--
--         , testListAliasesResponse $
--             listAliasesResponse
--
--         , testGenerateRandomResponse $
--             generateRandomResponse
--
--         , testCreateKeyResponse $
--             createKeyResponse
--
--         , testDisableKeyResponse $
--             disableKeyResponse
--
--         , testRetireGrantResponse $
--             retireGrantResponse
--
--         , testListKeysResponse $
--             listKeysResponse
--
--         , testGetKeyRotationStatusResponse $
--             getKeyRotationStatusResponse
--
--         , testGenerateDataKeyResponse $
--             generateDataKeyResponse
--
--         , testDeleteAliasResponse $
--             deleteAliasResponse
--
--         , testUpdateAliasResponse $
--             updateAliasResponse
--
--         , testDescribeKeyResponse $
--             describeKeyResponse
--
--         , testDecryptResponse $
--             decryptResponse
--
--         , testUpdateKeyDescriptionResponse $
--             updateKeyDescriptionResponse
--
--         , testReEncryptResponse $
--             reEncryptResponse
--
--         , testListKeyPoliciesResponse $
--             listKeyPoliciesResponse
--
--         , testPutKeyPolicyResponse $
--             putKeyPolicyResponse
--
--         , testEnableKeyResponse $
--             enableKeyResponse
--
--         , testRevokeGrantResponse $
--             revokeGrantResponse
--
--         , testGetKeyPolicyResponse $
--             getKeyPolicyResponse
--
--           ]
--     ]

-- Requests

testEncrypt :: Encrypt -> TestTree
testEncrypt = req
    "Encrypt"
    "fixture/Encrypt.yaml"

testListGrants :: ListGrants -> TestTree
testListGrants = req
    "ListGrants"
    "fixture/ListGrants.yaml"

testDisableKeyRotation :: DisableKeyRotation -> TestTree
testDisableKeyRotation = req
    "DisableKeyRotation"
    "fixture/DisableKeyRotation.yaml"

testGenerateDataKeyWithoutPlaintext :: GenerateDataKeyWithoutPlaintext -> TestTree
testGenerateDataKeyWithoutPlaintext = req
    "GenerateDataKeyWithoutPlaintext"
    "fixture/GenerateDataKeyWithoutPlaintext.yaml"

testEnableKeyRotation :: EnableKeyRotation -> TestTree
testEnableKeyRotation = req
    "EnableKeyRotation"
    "fixture/EnableKeyRotation.yaml"

testCreateAlias :: CreateAlias -> TestTree
testCreateAlias = req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

testCreateGrant :: CreateGrant -> TestTree
testCreateGrant = req
    "CreateGrant"
    "fixture/CreateGrant.yaml"

testListAliases :: ListAliases -> TestTree
testListAliases = req
    "ListAliases"
    "fixture/ListAliases.yaml"

testGenerateRandom :: GenerateRandom -> TestTree
testGenerateRandom = req
    "GenerateRandom"
    "fixture/GenerateRandom.yaml"

testCreateKey :: CreateKey -> TestTree
testCreateKey = req
    "CreateKey"
    "fixture/CreateKey.yaml"

testDisableKey :: DisableKey -> TestTree
testDisableKey = req
    "DisableKey"
    "fixture/DisableKey.yaml"

testRetireGrant :: RetireGrant -> TestTree
testRetireGrant = req
    "RetireGrant"
    "fixture/RetireGrant.yaml"

testListKeys :: ListKeys -> TestTree
testListKeys = req
    "ListKeys"
    "fixture/ListKeys.yaml"

testGetKeyRotationStatus :: GetKeyRotationStatus -> TestTree
testGetKeyRotationStatus = req
    "GetKeyRotationStatus"
    "fixture/GetKeyRotationStatus.yaml"

testGenerateDataKey :: GenerateDataKey -> TestTree
testGenerateDataKey = req
    "GenerateDataKey"
    "fixture/GenerateDataKey.yaml"

testDeleteAlias :: DeleteAlias -> TestTree
testDeleteAlias = req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

testUpdateAlias :: UpdateAlias -> TestTree
testUpdateAlias = req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

testDescribeKey :: DescribeKey -> TestTree
testDescribeKey = req
    "DescribeKey"
    "fixture/DescribeKey.yaml"

testDecrypt :: Decrypt -> TestTree
testDecrypt = req
    "Decrypt"
    "fixture/Decrypt.yaml"

testUpdateKeyDescription :: UpdateKeyDescription -> TestTree
testUpdateKeyDescription = req
    "UpdateKeyDescription"
    "fixture/UpdateKeyDescription.yaml"

testReEncrypt :: ReEncrypt -> TestTree
testReEncrypt = req
    "ReEncrypt"
    "fixture/ReEncrypt.yaml"

testListKeyPolicies :: ListKeyPolicies -> TestTree
testListKeyPolicies = req
    "ListKeyPolicies"
    "fixture/ListKeyPolicies.yaml"

testPutKeyPolicy :: PutKeyPolicy -> TestTree
testPutKeyPolicy = req
    "PutKeyPolicy"
    "fixture/PutKeyPolicy.yaml"

testEnableKey :: EnableKey -> TestTree
testEnableKey = req
    "EnableKey"
    "fixture/EnableKey.yaml"

testRevokeGrant :: RevokeGrant -> TestTree
testRevokeGrant = req
    "RevokeGrant"
    "fixture/RevokeGrant.yaml"

testGetKeyPolicy :: GetKeyPolicy -> TestTree
testGetKeyPolicy = req
    "GetKeyPolicy"
    "fixture/GetKeyPolicy.yaml"

-- Responses

testEncryptResponse :: EncryptResponse -> TestTree
testEncryptResponse = res
    "EncryptResponse"
    "fixture/EncryptResponse.proto"
    kMS
    (Proxy :: Proxy Encrypt)

testListGrantsResponse :: ListGrantsResponse -> TestTree
testListGrantsResponse = res
    "ListGrantsResponse"
    "fixture/ListGrantsResponse.proto"
    kMS
    (Proxy :: Proxy ListGrants)

testDisableKeyRotationResponse :: DisableKeyRotationResponse -> TestTree
testDisableKeyRotationResponse = res
    "DisableKeyRotationResponse"
    "fixture/DisableKeyRotationResponse.proto"
    kMS
    (Proxy :: Proxy DisableKeyRotation)

testGenerateDataKeyWithoutPlaintextResponse :: GenerateDataKeyWithoutPlaintextResponse -> TestTree
testGenerateDataKeyWithoutPlaintextResponse = res
    "GenerateDataKeyWithoutPlaintextResponse"
    "fixture/GenerateDataKeyWithoutPlaintextResponse.proto"
    kMS
    (Proxy :: Proxy GenerateDataKeyWithoutPlaintext)

testEnableKeyRotationResponse :: EnableKeyRotationResponse -> TestTree
testEnableKeyRotationResponse = res
    "EnableKeyRotationResponse"
    "fixture/EnableKeyRotationResponse.proto"
    kMS
    (Proxy :: Proxy EnableKeyRotation)

testCreateAliasResponse :: CreateAliasResponse -> TestTree
testCreateAliasResponse = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    kMS
    (Proxy :: Proxy CreateAlias)

testCreateGrantResponse :: CreateGrantResponse -> TestTree
testCreateGrantResponse = res
    "CreateGrantResponse"
    "fixture/CreateGrantResponse.proto"
    kMS
    (Proxy :: Proxy CreateGrant)

testListAliasesResponse :: ListAliasesResponse -> TestTree
testListAliasesResponse = res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    kMS
    (Proxy :: Proxy ListAliases)

testGenerateRandomResponse :: GenerateRandomResponse -> TestTree
testGenerateRandomResponse = res
    "GenerateRandomResponse"
    "fixture/GenerateRandomResponse.proto"
    kMS
    (Proxy :: Proxy GenerateRandom)

testCreateKeyResponse :: CreateKeyResponse -> TestTree
testCreateKeyResponse = res
    "CreateKeyResponse"
    "fixture/CreateKeyResponse.proto"
    kMS
    (Proxy :: Proxy CreateKey)

testDisableKeyResponse :: DisableKeyResponse -> TestTree
testDisableKeyResponse = res
    "DisableKeyResponse"
    "fixture/DisableKeyResponse.proto"
    kMS
    (Proxy :: Proxy DisableKey)

testRetireGrantResponse :: RetireGrantResponse -> TestTree
testRetireGrantResponse = res
    "RetireGrantResponse"
    "fixture/RetireGrantResponse.proto"
    kMS
    (Proxy :: Proxy RetireGrant)

testListKeysResponse :: ListKeysResponse -> TestTree
testListKeysResponse = res
    "ListKeysResponse"
    "fixture/ListKeysResponse.proto"
    kMS
    (Proxy :: Proxy ListKeys)

testGetKeyRotationStatusResponse :: GetKeyRotationStatusResponse -> TestTree
testGetKeyRotationStatusResponse = res
    "GetKeyRotationStatusResponse"
    "fixture/GetKeyRotationStatusResponse.proto"
    kMS
    (Proxy :: Proxy GetKeyRotationStatus)

testGenerateDataKeyResponse :: GenerateDataKeyResponse -> TestTree
testGenerateDataKeyResponse = res
    "GenerateDataKeyResponse"
    "fixture/GenerateDataKeyResponse.proto"
    kMS
    (Proxy :: Proxy GenerateDataKey)

testDeleteAliasResponse :: DeleteAliasResponse -> TestTree
testDeleteAliasResponse = res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    kMS
    (Proxy :: Proxy DeleteAlias)

testUpdateAliasResponse :: UpdateAliasResponse -> TestTree
testUpdateAliasResponse = res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    kMS
    (Proxy :: Proxy UpdateAlias)

testDescribeKeyResponse :: DescribeKeyResponse -> TestTree
testDescribeKeyResponse = res
    "DescribeKeyResponse"
    "fixture/DescribeKeyResponse.proto"
    kMS
    (Proxy :: Proxy DescribeKey)

testDecryptResponse :: DecryptResponse -> TestTree
testDecryptResponse = res
    "DecryptResponse"
    "fixture/DecryptResponse.proto"
    kMS
    (Proxy :: Proxy Decrypt)

testUpdateKeyDescriptionResponse :: UpdateKeyDescriptionResponse -> TestTree
testUpdateKeyDescriptionResponse = res
    "UpdateKeyDescriptionResponse"
    "fixture/UpdateKeyDescriptionResponse.proto"
    kMS
    (Proxy :: Proxy UpdateKeyDescription)

testReEncryptResponse :: ReEncryptResponse -> TestTree
testReEncryptResponse = res
    "ReEncryptResponse"
    "fixture/ReEncryptResponse.proto"
    kMS
    (Proxy :: Proxy ReEncrypt)

testListKeyPoliciesResponse :: ListKeyPoliciesResponse -> TestTree
testListKeyPoliciesResponse = res
    "ListKeyPoliciesResponse"
    "fixture/ListKeyPoliciesResponse.proto"
    kMS
    (Proxy :: Proxy ListKeyPolicies)

testPutKeyPolicyResponse :: PutKeyPolicyResponse -> TestTree
testPutKeyPolicyResponse = res
    "PutKeyPolicyResponse"
    "fixture/PutKeyPolicyResponse.proto"
    kMS
    (Proxy :: Proxy PutKeyPolicy)

testEnableKeyResponse :: EnableKeyResponse -> TestTree
testEnableKeyResponse = res
    "EnableKeyResponse"
    "fixture/EnableKeyResponse.proto"
    kMS
    (Proxy :: Proxy EnableKey)

testRevokeGrantResponse :: RevokeGrantResponse -> TestTree
testRevokeGrantResponse = res
    "RevokeGrantResponse"
    "fixture/RevokeGrantResponse.proto"
    kMS
    (Proxy :: Proxy RevokeGrant)

testGetKeyPolicyResponse :: GetKeyPolicyResponse -> TestTree
testGetKeyPolicyResponse = res
    "GetKeyPolicyResponse"
    "fixture/GetKeyPolicyResponse.proto"
    kMS
    (Proxy :: Proxy GetKeyPolicy)
