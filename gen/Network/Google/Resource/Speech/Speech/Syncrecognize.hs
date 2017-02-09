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
-- Module      : Network.Google.Resource.Speech.Speech.Syncrecognize
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Perform synchronous speech-recognition: receive results after all audio
-- has been sent and processed.
--
-- /See:/ <https://cloud.google.com/speech/ Google Cloud Speech API Reference> for @speech.speech.syncrecognize@.
module Network.Google.Resource.Speech.Speech.Syncrecognize
    (
    -- * REST Resource
      SpeechSyncrecognizeResource

    -- * Creating a Request
    , speechSyncrecognize
    , SpeechSyncrecognize

    -- * Request Lenses
    , ssXgafv
    , ssUploadProtocol
    , ssPp
    , ssAccessToken
    , ssUploadType
    , ssPayload
    , ssBearerToken
    , ssCallback
    ) where

import Network.Google.Prelude
import Network.Google.Speech.Types

-- | A resource alias for @speech.speech.syncrecognize@ method which the
-- 'SpeechSyncrecognize' request conforms to.
type SpeechSyncrecognizeResource =
     "v1beta1" :>
       "speech:syncrecognize" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "alt" AltJSON :>
                         ReqBody '[JSON] SyncRecognizeRequest :>
                           Post '[JSON] SyncRecognizeResponse

-- | Perform synchronous speech-recognition: receive results after all audio
-- has been sent and processed.
--
-- /See:/ 'speechSyncrecognize' smart constructor.
data SpeechSyncrecognize = SpeechSyncrecognize'
    { _ssXgafv :: !(Maybe Xgafv)
    , _ssUploadProtocol :: !(Maybe Text)
    , _ssPp :: !Bool
    , _ssAccessToken :: !(Maybe Text)
    , _ssUploadType :: !(Maybe Text)
    , _ssPayload :: !SyncRecognizeRequest
    , _ssBearerToken :: !(Maybe Text)
    , _ssCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SpeechSyncrecognize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssXgafv'
--
-- * 'ssUploadProtocol'
--
-- * 'ssPp'
--
-- * 'ssAccessToken'
--
-- * 'ssUploadType'
--
-- * 'ssPayload'
--
-- * 'ssBearerToken'
--
-- * 'ssCallback'
speechSyncrecognize
    :: SyncRecognizeRequest -- ^ 'ssPayload'
    -> SpeechSyncrecognize
speechSyncrecognize pSsPayload_ = 
    SpeechSyncrecognize'
    { _ssXgafv = Nothing
    , _ssUploadProtocol = Nothing
    , _ssPp = True
    , _ssAccessToken = Nothing
    , _ssUploadType = Nothing
    , _ssPayload = pSsPayload_
    , _ssBearerToken = Nothing
    , _ssCallback = Nothing
    }

-- | V1 error format.
ssXgafv :: Lens' SpeechSyncrecognize (Maybe Xgafv)
ssXgafv = lens _ssXgafv (\ s a -> s{_ssXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ssUploadProtocol :: Lens' SpeechSyncrecognize (Maybe Text)
ssUploadProtocol
  = lens _ssUploadProtocol
      (\ s a -> s{_ssUploadProtocol = a})

-- | Pretty-print response.
ssPp :: Lens' SpeechSyncrecognize Bool
ssPp = lens _ssPp (\ s a -> s{_ssPp = a})

-- | OAuth access token.
ssAccessToken :: Lens' SpeechSyncrecognize (Maybe Text)
ssAccessToken
  = lens _ssAccessToken
      (\ s a -> s{_ssAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ssUploadType :: Lens' SpeechSyncrecognize (Maybe Text)
ssUploadType
  = lens _ssUploadType (\ s a -> s{_ssUploadType = a})

-- | Multipart request metadata.
ssPayload :: Lens' SpeechSyncrecognize SyncRecognizeRequest
ssPayload
  = lens _ssPayload (\ s a -> s{_ssPayload = a})

-- | OAuth bearer token.
ssBearerToken :: Lens' SpeechSyncrecognize (Maybe Text)
ssBearerToken
  = lens _ssBearerToken
      (\ s a -> s{_ssBearerToken = a})

-- | JSONP
ssCallback :: Lens' SpeechSyncrecognize (Maybe Text)
ssCallback
  = lens _ssCallback (\ s a -> s{_ssCallback = a})

instance GoogleRequest SpeechSyncrecognize where
        type Rs SpeechSyncrecognize = SyncRecognizeResponse
        type Scopes SpeechSyncrecognize =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient SpeechSyncrecognize'{..}
          = go _ssXgafv _ssUploadProtocol (Just _ssPp)
              _ssAccessToken
              _ssUploadType
              _ssBearerToken
              _ssCallback
              (Just AltJSON)
              _ssPayload
              speechService
          where go
                  = buildClient
                      (Proxy :: Proxy SpeechSyncrecognizeResource)
                      mempty
