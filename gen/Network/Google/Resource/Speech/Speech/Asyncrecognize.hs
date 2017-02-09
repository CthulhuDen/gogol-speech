{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.Speech.Speech.Asyncrecognize
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Perform asynchronous speech-recognition: receive results via the
-- google.longrunning.Operations interface. Returns either an
-- \`Operation.error\` or an \`Operation.response\` which contains an
-- \`AsyncRecognizeResponse\` message.
--
-- /See:/ <https://cloud.google.com/speech/ Google Cloud Speech API Reference> for @speech.speech.asyncrecognize@.
module Network.Google.Resource.Speech.Speech.Asyncrecognize
    (
    -- * REST Resource
      SpeechAsyncrecognizeResource

    -- * Creating a Request
    , speechAsyncrecognize
    , SpeechAsyncrecognize

    -- * Request Lenses
    , saXgafv
    , saUploadProtocol
    , saPp
    , saAccessToken
    , saUploadType
    , saPayload
    , saBearerToken
    , saCallback
    ) where

import Network.Google.Prelude
import Network.Google.Speech.Types

-- | A resource alias for @speech.speech.asyncrecognize@ method which the
-- 'SpeechAsyncrecognize' request conforms to.
type SpeechAsyncrecognizeResource =
     "v1beta1" :>
       "speech:asyncrecognize" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :>
                         ReqBody '[JSON] AsyncRecognizeRequest :>
                           Post '[JSON] Operation

-- | Perform asynchronous speech-recognition: receive results via the
-- google.longrunning.Operations interface. Returns either an
-- \`Operation.error\` or an \`Operation.response\` which contains an
-- \`AsyncRecognizeResponse\` message.
--
-- /See:/ 'speechAsyncrecognize' smart constructor.
data SpeechAsyncrecognize = SpeechAsyncrecognize'
    { _saXgafv :: !(Maybe Xgafv)
    , _saUploadProtocol :: !(Maybe Text)
    , _saPp :: !Bool
    , _saAccessToken :: !(Maybe Text)
    , _saUploadType :: !(Maybe Text)
    , _saPayload :: !AsyncRecognizeRequest
    , _saBearerToken :: !(Maybe Text)
    , _saCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SpeechAsyncrecognize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saXgafv'
--
-- * 'saUploadProtocol'
--
-- * 'saPp'
--
-- * 'saAccessToken'
--
-- * 'saUploadType'
--
-- * 'saPayload'
--
-- * 'saBearerToken'
--
-- * 'saCallback'
speechAsyncrecognize
    :: AsyncRecognizeRequest -- ^ 'saPayload'
    -> SpeechAsyncrecognize
speechAsyncrecognize pSaPayload_ = 
    SpeechAsyncrecognize'
    { _saXgafv = Nothing
    , _saUploadProtocol = Nothing
    , _saPp = True
    , _saAccessToken = Nothing
    , _saUploadType = Nothing
    , _saPayload = pSaPayload_
    , _saBearerToken = Nothing
    , _saCallback = Nothing
    }

-- | V1 error format.
saXgafv :: Lens' SpeechAsyncrecognize (Maybe Xgafv)
saXgafv = lens _saXgafv (\ s a -> s{_saXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
saUploadProtocol :: Lens' SpeechAsyncrecognize (Maybe Text)
saUploadProtocol
  = lens _saUploadProtocol
      (\ s a -> s{_saUploadProtocol = a})

-- | Pretty-print response.
saPp :: Lens' SpeechAsyncrecognize Bool
saPp = lens _saPp (\ s a -> s{_saPp = a})

-- | OAuth access token.
saAccessToken :: Lens' SpeechAsyncrecognize (Maybe Text)
saAccessToken
  = lens _saAccessToken
      (\ s a -> s{_saAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
saUploadType :: Lens' SpeechAsyncrecognize (Maybe Text)
saUploadType
  = lens _saUploadType (\ s a -> s{_saUploadType = a})

-- | Multipart request metadata.
saPayload :: Lens' SpeechAsyncrecognize AsyncRecognizeRequest
saPayload
  = lens _saPayload (\ s a -> s{_saPayload = a})

-- | OAuth bearer token.
saBearerToken :: Lens' SpeechAsyncrecognize (Maybe Text)
saBearerToken
  = lens _saBearerToken
      (\ s a -> s{_saBearerToken = a})

-- | JSONP
saCallback :: Lens' SpeechAsyncrecognize (Maybe Text)
saCallback
  = lens _saCallback (\ s a -> s{_saCallback = a})

instance GoogleRequest SpeechAsyncrecognize where
        type Rs SpeechAsyncrecognize = Operation
        type Scopes SpeechAsyncrecognize =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient SpeechAsyncrecognize'{..}
          = go _saXgafv _saUploadProtocol (Just _saPp)
              _saAccessToken
              _saUploadType
              _saBearerToken
              _saCallback
              (Just AltJSON)
              _saPayload
              speechService
          where go
                  = buildClient
                      (Proxy :: Proxy SpeechAsyncrecognizeResource)
                      mempty
