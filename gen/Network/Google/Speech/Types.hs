{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Speech.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Speech.Types
    (
    -- * Service Configuration
      speechService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- * SpeechContext
    , SpeechContext
    , speechContext
    , scPhrases

    -- * ListOperationsResponse
    , ListOperationsResponse
    , listOperationsResponse
    , lorNextPageToken
    , lorOperations

    -- * CancelOperationRequest
    , CancelOperationRequest
    , cancelOperationRequest

    -- * Operation
    , Operation
    , operation
    , oDone
    , oError
    , oResponse
    , oName
    , oMetadata

    -- * Empty
    , Empty
    , empty

    -- * SpeechRecognitionAlternative
    , SpeechRecognitionAlternative
    , speechRecognitionAlternative
    , sraConfidence
    , sraTranscript

    -- * StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- * SpeechRecognitionResult
    , SpeechRecognitionResult
    , speechRecognitionResult
    , srrAlternatives

    -- * RecognitionAudio
    , RecognitionAudio
    , recognitionAudio
    , raURI
    , raContent

    -- * Xgafv
    , Xgafv (..)

    -- * SyncRecognizeRequest
    , SyncRecognizeRequest
    , syncRecognizeRequest
    , srrConfig
    , srrAudio

    -- * RecognitionConfig
    , RecognitionConfig
    , recognitionConfig
    , rcLanguageCode
    , rcSpeechContext
    , rcMaxAlternatives
    , rcSampleRate
    , rcProfanityFilter
    , rcEncoding

    -- * SyncRecognizeResponse
    , SyncRecognizeResponse
    , syncRecognizeResponse
    , srrResults

    -- * OperationMetadata
    , OperationMetadata
    , operationMetadata
    , omAddtional

    -- * AsyncRecognizeRequest
    , AsyncRecognizeRequest
    , asyncRecognizeRequest
    , arrConfig
    , arrAudio

    -- * RecognitionConfigEncoding
    , RecognitionConfigEncoding (..)

    -- * OperationResponse
    , OperationResponse
    , operationResponse
    , orAddtional
    ) where

import Network.Google.Prelude
import Network.Google.Speech.Types.Product
import Network.Google.Speech.Types.Sum

-- | Default request referring to version 'v1beta1' of the Google Cloud Speech API. This contains the host and root path used as a starting point for constructing service requests.
speechService :: ServiceConfig
speechService
  = defaultService (ServiceId "speech:v1beta1")
      "speech.googleapis.com"

-- | View and manage your data across Google Cloud Platform services
cloudPlatformScope :: Proxy '["https://www.googleapis.com/auth/cloud-platform"]
cloudPlatformScope = Proxy;
