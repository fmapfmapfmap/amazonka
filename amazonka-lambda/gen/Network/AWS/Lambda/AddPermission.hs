{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.AddPermission
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a permission to the access policy associated with the specified AWS
-- Lambda function. In a \"push event\" model, the access policy attached
-- to the Lambda function grants Amazon S3 or a user application permission
-- for the Lambda 'lambda:Invoke' action. For information about the push
-- model, see
-- <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works>.
-- Each Lambda function has one access policy associated with it. You can
-- use the 'AddPermission' API to add a permission to the policy. You have
-- one access policy but it can have multiple permission statements.
--
-- This operation requires permission for the 'lambda:AddPermission'
-- action.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_AddPermission.html AWS API Reference> for AddPermission.
module Network.AWS.Lambda.AddPermission
    (
    -- * Creating a Request
      addPermission
    , AddPermission
    -- * Request Lenses
    , apSourceAccount
    , apSourceARN
    , apFunctionName
    , apStatementId
    , apAction
    , apPrincipal

    -- * Destructuring the Response
    , addPermissionResponse
    , AddPermissionResponse
    -- * Response Lenses
    , aprsStatement
    , aprsResponseStatus
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addPermission' smart constructor.
data AddPermission = AddPermission'
    { _apSourceAccount :: !(Maybe Text)
    , _apSourceARN     :: !(Maybe Text)
    , _apFunctionName  :: !Text
    , _apStatementId   :: !Text
    , _apAction        :: !Text
    , _apPrincipal     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apSourceAccount'
--
-- * 'apSourceARN'
--
-- * 'apFunctionName'
--
-- * 'apStatementId'
--
-- * 'apAction'
--
-- * 'apPrincipal'
addPermission
    :: Text -- ^ 'apFunctionName'
    -> Text -- ^ 'apStatementId'
    -> Text -- ^ 'apAction'
    -> Text -- ^ 'apPrincipal'
    -> AddPermission
addPermission pFunctionName_ pStatementId_ pAction_ pPrincipal_ =
    AddPermission'
    { _apSourceAccount = Nothing
    , _apSourceARN = Nothing
    , _apFunctionName = pFunctionName_
    , _apStatementId = pStatementId_
    , _apAction = pAction_
    , _apPrincipal = pPrincipal_
    }

-- | The AWS account ID (without a hyphen) of the source owner. For example,
-- if the 'SourceArn' identifies a bucket, then this is the bucket owner\'s
-- account ID. You can use this additional condition to ensure the bucket
-- you specify is owned by a specific account (it is possible the bucket
-- owner deleted the bucket and some other AWS account created the bucket).
-- You can also use this condition to specify all sources (that is, you
-- don\'t specify the 'SourceArn') owned by a specific account.
apSourceAccount :: Lens' AddPermission (Maybe Text)
apSourceAccount = lens _apSourceAccount (\ s a -> s{_apSourceAccount = a});

-- | This is optional; however, when granting Amazon S3 permission to invoke
-- your function, you should specify this field with the bucket Amazon
-- Resource Name (ARN) as its value. This ensures that only events
-- generated from the specified bucket can invoke the function.
--
-- If you add a permission for the Amazon S3 principal without providing
-- the source ARN, any AWS account that creates a mapping to your function
-- ARN can send events to invoke your Lambda function from Amazon S3.
apSourceARN :: Lens' AddPermission (Maybe Text)
apSourceARN = lens _apSourceARN (\ s a -> s{_apSourceARN = a});

-- | Name of the Lambda function whose access policy you are updating by
-- adding a new permission.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
apFunctionName :: Lens' AddPermission Text
apFunctionName = lens _apFunctionName (\ s a -> s{_apFunctionName = a});

-- | A unique statement identifier.
apStatementId :: Lens' AddPermission Text
apStatementId = lens _apStatementId (\ s a -> s{_apStatementId = a});

-- | The AWS Lambda action you want to allow in this statement. Each Lambda
-- action is a string starting with \"lambda:\" followed by the API name
-- (see Operations). For example, \"lambda:CreateFunction\". You can use
-- wildcard (\"lambda:*\") to grant permission for all AWS Lambda actions.
apAction :: Lens' AddPermission Text
apAction = lens _apAction (\ s a -> s{_apAction = a});

-- | The principal who is getting this permission. It can be Amazon S3
-- service Principal (\"s3.amazonaws.com\") if you want Amazon S3 to invoke
-- the function, an AWS account ID if you are granting cross-account
-- permission, or any valid AWS service principal such as
-- \"sns.amazonaws.com\". For example, you might want to allow a custom
-- application in another AWS account to push events to AWS Lambda by
-- invoking your function.
apPrincipal :: Lens' AddPermission Text
apPrincipal = lens _apPrincipal (\ s a -> s{_apPrincipal = a});

instance AWSRequest AddPermission where
        type Rs AddPermission = AddPermissionResponse
        request = postJSON lambda
        response
          = receiveJSON
              (\ s h x ->
                 AddPermissionResponse' <$>
                   (x .?> "Statement") <*> (pure (fromEnum s)))

instance ToHeaders AddPermission where
        toHeaders = const mempty

instance ToJSON AddPermission where
        toJSON AddPermission'{..}
          = object
              (catMaybes
                 [("SourceAccount" .=) <$> _apSourceAccount,
                  ("SourceArn" .=) <$> _apSourceARN,
                  Just ("StatementId" .= _apStatementId),
                  Just ("Action" .= _apAction),
                  Just ("Principal" .= _apPrincipal)])

instance ToPath AddPermission where
        toPath AddPermission'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _apFunctionName,
               "/versions/HEAD/policy"]

instance ToQuery AddPermission where
        toQuery = const mempty

-- | /See:/ 'addPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
    { _aprsStatement      :: !(Maybe Text)
    , _aprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddPermissionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aprsStatement'
--
-- * 'aprsResponseStatus'
addPermissionResponse
    :: Int -- ^ 'aprsResponseStatus'
    -> AddPermissionResponse
addPermissionResponse pResponseStatus_ =
    AddPermissionResponse'
    { _aprsStatement = Nothing
    , _aprsResponseStatus = pResponseStatus_
    }

-- | The permission statement you specified in the request. The response
-- returns the same as a string using \"\\\" as an escape character in the
-- JSON.
aprsStatement :: Lens' AddPermissionResponse (Maybe Text)
aprsStatement = lens _aprsStatement (\ s a -> s{_aprsStatement = a});

-- | The response status code.
aprsResponseStatus :: Lens' AddPermissionResponse Int
aprsResponseStatus = lens _aprsResponseStatus (\ s a -> s{_aprsResponseStatus = a});
