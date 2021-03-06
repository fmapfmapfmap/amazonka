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
-- Module      : Network.AWS.SSM.ListAssociations
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the associations for the specified configuration document or
-- instance.
--
-- /See:/ <http://docs.aws.amazon.com/ssm/latest/APIReference/API_ListAssociations.html AWS API Reference> for ListAssociations.
module Network.AWS.SSM.ListAssociations
    (
    -- * Creating a Request
      listAssociations
    , ListAssociations
    -- * Request Lenses
    , laNextToken
    , laMaxResults
    , laAssociationFilterList

    -- * Destructuring the Response
    , listAssociationsResponse
    , ListAssociationsResponse
    -- * Response Lenses
    , larsNextToken
    , larsAssociations
    , larsResponseStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'listAssociations' smart constructor.
data ListAssociations = ListAssociations'
    { _laNextToken             :: !(Maybe Text)
    , _laMaxResults            :: !(Maybe Nat)
    , _laAssociationFilterList :: !(List1 AssociationFilter)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNextToken'
--
-- * 'laMaxResults'
--
-- * 'laAssociationFilterList'
listAssociations
    :: NonEmpty AssociationFilter -- ^ 'laAssociationFilterList'
    -> ListAssociations
listAssociations pAssociationFilterList_ =
    ListAssociations'
    { _laNextToken = Nothing
    , _laMaxResults = Nothing
    , _laAssociationFilterList = _List1 # pAssociationFilterList_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
laNextToken :: Lens' ListAssociations (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a});

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
laMaxResults :: Lens' ListAssociations (Maybe Natural)
laMaxResults = lens _laMaxResults (\ s a -> s{_laMaxResults = a}) . mapping _Nat;

-- | One or more filters. Use a filter to return a more specific list of
-- results.
laAssociationFilterList :: Lens' ListAssociations (NonEmpty AssociationFilter)
laAssociationFilterList = lens _laAssociationFilterList (\ s a -> s{_laAssociationFilterList = a}) . _List1;

instance AWSRequest ListAssociations where
        type Rs ListAssociations = ListAssociationsResponse
        request = postJSON sSM
        response
          = receiveJSON
              (\ s h x ->
                 ListAssociationsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Associations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListAssociations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListAssociations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAssociations where
        toJSON ListAssociations'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _laNextToken,
                  ("MaxResults" .=) <$> _laMaxResults,
                  Just
                    ("AssociationFilterList" .=
                       _laAssociationFilterList)])

instance ToPath ListAssociations where
        toPath = const "/"

instance ToQuery ListAssociations where
        toQuery = const mempty

-- | /See:/ 'listAssociationsResponse' smart constructor.
data ListAssociationsResponse = ListAssociationsResponse'
    { _larsNextToken      :: !(Maybe Text)
    , _larsAssociations   :: !(Maybe [Association])
    , _larsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsNextToken'
--
-- * 'larsAssociations'
--
-- * 'larsResponseStatus'
listAssociationsResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAssociationsResponse
listAssociationsResponse pResponseStatus_ =
    ListAssociationsResponse'
    { _larsNextToken = Nothing
    , _larsAssociations = Nothing
    , _larsResponseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
larsNextToken :: Lens' ListAssociationsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a});

-- | The associations.
larsAssociations :: Lens' ListAssociationsResponse [Association]
larsAssociations = lens _larsAssociations (\ s a -> s{_larsAssociations = a}) . _Default . _Coerce;

-- | The response status code.
larsResponseStatus :: Lens' ListAssociationsResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a});
