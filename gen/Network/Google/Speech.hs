{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Speech
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Google Cloud Speech API.
--
-- /See:/ <https://cloud.google.com/speech/ Google Cloud Speech API Reference>
module Network.Google.Speech
    (
    -- * Service Configuration
      speechService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * API Declaration
    , SpeechAPI

    -- * Resources

    -- ** speech.operations.cancel
    , module Network.Google.Resource.Speech.Operations.Cancel

    -- ** speech.operations.delete
    , module Network.Google.Resource.Speech.Operations.Delete

    -- ** speech.operations.get
    , module Network.Google.Resource.Speech.Operations.Get

    -- ** speech.operations.list
    , module Network.Google.Resource.Speech.Operations.List

    -- ** speech.speech.asyncrecognize
    , module Network.Google.Resource.Speech.Speech.Asyncrecognize

    -- ** speech.speech.syncrecognize
    , module Network.Google.Resource.Speech.Speech.Syncrecognize

    -- * Types

    -- ** Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- ** SpeechContext
    , SpeechContext
    , speechContext
    , scPhrases

    -- ** ListOperationsResponse
    , ListOperationsResponse
    , listOperationsResponse
    , lorNextPageToken
    , lorOperations

    -- ** CancelOperationRequest
    , CancelOperationRequest
    , cancelOperationRequest

    -- ** Operation
    , Operation
    , operation
    , oDone
    , oError
    , oResponse
    , oName
    , oMetadata

    -- ** Empty
    , Empty
    , empty

    -- ** SpeechRecognitionAlternative
    , SpeechRecognitionAlternative
    , speechRecognitionAlternative
    , sraConfidence
    , sraTranscript

    -- ** StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- ** SpeechRecognitionResult
    , SpeechRecognitionResult
    , speechRecognitionResult
    , srrAlternatives

    -- ** RecognitionAudio
    , RecognitionAudio
    , recognitionAudio
    , raURI
    , raContent

    -- ** Xgafv
    , Xgafv (..)

    -- ** SyncRecognizeRequest
    , SyncRecognizeRequest
    , syncRecognizeRequest
    , srrConfig
    , srrAudio

    -- ** RecognitionConfig
    , RecognitionConfig
    , recognitionConfig
    , rcLanguageCode
    , rcSpeechContext
    , rcMaxAlternatives
    , rcSampleRate
    , rcProfanityFilter
    , rcEncoding

    -- ** SyncRecognizeResponse
    , SyncRecognizeResponse
    , syncRecognizeResponse
    , srrResults

    -- ** OperationMetadata
    , OperationMetadata
    , operationMetadata
    , omAddtional

    -- ** AsyncRecognizeRequest
    , AsyncRecognizeRequest
    , asyncRecognizeRequest
    , arrConfig
    , arrAudio

    -- ** RecognitionConfigEncoding
    , RecognitionConfigEncoding (..)

    -- ** OperationResponse
    , OperationResponse
    , operationResponse
    , orAddtional
    ) where

import Network.Google.Prelude
import Network.Google.Resource.Speech.Operations.Cancel
import Network.Google.Resource.Speech.Operations.Delete
import Network.Google.Resource.Speech.Operations.Get
import Network.Google.Resource.Speech.Operations.List
import Network.Google.Resource.Speech.Speech.Asyncrecognize
import Network.Google.Resource.Speech.Speech.Syncrecognize
import Network.Google.Speech.Types

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Cloud Speech API service.
type SpeechAPI =
     SpeechAsyncrecognizeResource :<|>
       SpeechSyncrecognizeResource
       :<|> OperationsListResource
       :<|> OperationsGetResource
       :<|> OperationsCancelResource
       :<|> OperationsDeleteResource
