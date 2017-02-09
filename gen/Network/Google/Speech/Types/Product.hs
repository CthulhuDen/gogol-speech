{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Speech.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Speech.Types.Product where

import Network.Google.Prelude
import Network.Google.Speech.Types.Sum

-- | The \`Status\` type defines a logical error model that is suitable for
-- different programming environments, including REST APIs and RPC APIs. It
-- is used by [gRPC](https:\/\/github.com\/grpc). The error model is
-- designed to be: - Simple to use and understand for most users - Flexible
-- enough to meet unexpected needs # Overview The \`Status\` message
-- contains three pieces of data: error code, error message, and error
-- details. The error code should be an enum value of google.rpc.Code, but
-- it may accept additional error codes if needed. The error message should
-- be a developer-facing English message that helps developers *understand*
-- and *resolve* the error. If a localized user-facing error message is
-- needed, put the localized message in the error details or localize it in
-- the client. The optional error details may contain arbitrary information
-- about the error. There is a predefined set of error detail types in the
-- package \`google.rpc\` which can be used for common error conditions. #
-- Language mapping The \`Status\` message is the logical representation of
-- the error model, but it is not necessarily the actual wire format. When
-- the \`Status\` message is exposed in different client libraries and
-- different wire protocols, it can be mapped differently. For example, it
-- will likely be mapped to some exceptions in Java, but more likely mapped
-- to some error codes in C. # Other uses The error model and the
-- \`Status\` message can be used in a variety of environments, either with
-- or without APIs, to provide a consistent developer experience across
-- different environments. Example uses of this error model include: -
-- Partial errors. If a service needs to return partial errors to the
-- client, it may embed the \`Status\` in the normal response to indicate
-- the partial errors. - Workflow errors. A typical workflow has multiple
-- steps. Each step may have a \`Status\` message for error reporting
-- purpose. - Batch operations. If a client uses batch request and batch
-- response, the \`Status\` message should be used directly inside batch
-- response, one for each error sub-response. - Asynchronous operations. If
-- an API call embeds asynchronous operation results in its response, the
-- status of those operations should be represented directly using the
-- \`Status\` message. - Logging. If some API errors are stored in logs,
-- the message \`Status\` could be used directly after any stripping needed
-- for security\/privacy reasons.
--
-- /See:/ 'status' smart constructor.
data Status = Status'
    { _sDetails :: !(Maybe [StatusDetailsItem])
    , _sCode :: !(Maybe (Textual Int32))
    , _sMessage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Status' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDetails'
--
-- * 'sCode'
--
-- * 'sMessage'
status
    :: Status
status = 
    Status'
    { _sDetails = Nothing
    , _sCode = Nothing
    , _sMessage = Nothing
    }

-- | A list of messages that carry the error details. There will be a common
-- set of message types for APIs to use.
sDetails :: Lens' Status [StatusDetailsItem]
sDetails
  = lens _sDetails (\ s a -> s{_sDetails = a}) .
      _Default
      . _Coerce

-- | The status code, which should be an enum value of google.rpc.Code.
sCode :: Lens' Status (Maybe Int32)
sCode
  = lens _sCode (\ s a -> s{_sCode = a}) .
      mapping _Coerce

-- | A developer-facing error message, which should be in English. Any
-- user-facing error message should be localized and sent in the
-- google.rpc.Status.details field, or localized by the client.
sMessage :: Lens' Status (Maybe Text)
sMessage = lens _sMessage (\ s a -> s{_sMessage = a})

instance FromJSON Status where
        parseJSON
          = withObject "Status"
              (\ o ->
                 Status' <$>
                   (o .:? "details" .!= mempty) <*> (o .:? "code") <*>
                     (o .:? "message"))

instance ToJSON Status where
        toJSON Status'{..}
          = object
              (catMaybes
                 [("details" .=) <$> _sDetails,
                  ("code" .=) <$> _sCode,
                  ("message" .=) <$> _sMessage])

-- | Provides \"hints\" to the speech recognizer to favor specific words and
-- phrases in the results.
--
-- /See:/ 'speechContext' smart constructor.
newtype SpeechContext = SpeechContext'
    { _scPhrases :: Maybe [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SpeechContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scPhrases'
speechContext
    :: SpeechContext
speechContext = 
    SpeechContext'
    { _scPhrases = Nothing
    }

-- | [Optional] A list of strings containing words and phrases \"hints\" so
-- that the speech recognition is more likely to recognize them. This can
-- be used to improve the accuracy for specific words and phrases, for
-- example, if specific commands are typically spoken by the user. This can
-- also be used to add additional words to the vocabulary of the
-- recognizer. See [usage
-- limits](https:\/\/cloud.google.com\/speech\/limits#content).
scPhrases :: Lens' SpeechContext [Text]
scPhrases
  = lens _scPhrases (\ s a -> s{_scPhrases = a}) .
      _Default
      . _Coerce

instance FromJSON SpeechContext where
        parseJSON
          = withObject "SpeechContext"
              (\ o ->
                 SpeechContext' <$> (o .:? "phrases" .!= mempty))

instance ToJSON SpeechContext where
        toJSON SpeechContext'{..}
          = object (catMaybes [("phrases" .=) <$> _scPhrases])

-- | The response message for Operations.ListOperations.
--
-- /See:/ 'listOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
    { _lorNextPageToken :: !(Maybe Text)
    , _lorOperations :: !(Maybe [Operation])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorNextPageToken'
--
-- * 'lorOperations'
listOperationsResponse
    :: ListOperationsResponse
listOperationsResponse = 
    ListOperationsResponse'
    { _lorNextPageToken = Nothing
    , _lorOperations = Nothing
    }

-- | The standard List next-page token.
lorNextPageToken :: Lens' ListOperationsResponse (Maybe Text)
lorNextPageToken
  = lens _lorNextPageToken
      (\ s a -> s{_lorNextPageToken = a})

-- | A list of operations that matches the specified filter in the request.
lorOperations :: Lens' ListOperationsResponse [Operation]
lorOperations
  = lens _lorOperations
      (\ s a -> s{_lorOperations = a})
      . _Default
      . _Coerce

instance FromJSON ListOperationsResponse where
        parseJSON
          = withObject "ListOperationsResponse"
              (\ o ->
                 ListOperationsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "operations" .!= mempty))

instance ToJSON ListOperationsResponse where
        toJSON ListOperationsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lorNextPageToken,
                  ("operations" .=) <$> _lorOperations])

-- | The request message for Operations.CancelOperation.
--
-- /See:/ 'cancelOperationRequest' smart constructor.
data CancelOperationRequest =
    CancelOperationRequest' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelOperationRequest' with the minimum fields required to make a request.
--
cancelOperationRequest
    :: CancelOperationRequest
cancelOperationRequest = CancelOperationRequest'

instance FromJSON CancelOperationRequest where
        parseJSON
          = withObject "CancelOperationRequest"
              (\ o -> pure CancelOperationRequest')

instance ToJSON CancelOperationRequest where
        toJSON = const emptyObject

-- | This resource represents a long-running operation that is the result of
-- a network API call.
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'
    { _oDone :: !(Maybe Bool)
    , _oError :: !(Maybe Status)
    , _oResponse :: !(Maybe OperationResponse)
    , _oName :: !(Maybe Text)
    , _oMetadata :: !(Maybe OperationMetadata)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oDone'
--
-- * 'oError'
--
-- * 'oResponse'
--
-- * 'oName'
--
-- * 'oMetadata'
operation
    :: Operation
operation = 
    Operation'
    { _oDone = Nothing
    , _oError = Nothing
    , _oResponse = Nothing
    , _oName = Nothing
    , _oMetadata = Nothing
    }

-- | If the value is \`false\`, it means the operation is still in progress.
-- If true, the operation is completed, and either \`error\` or
-- \`response\` is available.
oDone :: Lens' Operation (Maybe Bool)
oDone = lens _oDone (\ s a -> s{_oDone = a})

-- | The error result of the operation in case of failure or cancellation.
oError :: Lens' Operation (Maybe Status)
oError = lens _oError (\ s a -> s{_oError = a})

-- | The normal response of the operation in case of success. If the original
-- method returns no data on success, such as \`Delete\`, the response is
-- \`google.protobuf.Empty\`. If the original method is standard
-- \`Get\`\/\`Create\`\/\`Update\`, the response should be the resource.
-- For other methods, the response should have the type \`XxxResponse\`,
-- where \`Xxx\` is the original method name. For example, if the original
-- method name is \`TakeSnapshot()\`, the inferred response type is
-- \`TakeSnapshotResponse\`.
oResponse :: Lens' Operation (Maybe OperationResponse)
oResponse
  = lens _oResponse (\ s a -> s{_oResponse = a})

-- | The server-assigned name, which is only unique within the same service
-- that originally returns it. If you use the default HTTP mapping, the
-- \`name\` should have the format of \`operations\/some\/unique\/name\`.
oName :: Lens' Operation (Maybe Text)
oName = lens _oName (\ s a -> s{_oName = a})

-- | Service-specific metadata associated with the operation. It typically
-- contains progress information and common metadata such as create time.
-- Some services might not provide such metadata. Any method that returns a
-- long-running operation should document the metadata type, if any.
oMetadata :: Lens' Operation (Maybe OperationMetadata)
oMetadata
  = lens _oMetadata (\ s a -> s{_oMetadata = a})

instance FromJSON Operation where
        parseJSON
          = withObject "Operation"
              (\ o ->
                 Operation' <$>
                   (o .:? "done") <*> (o .:? "error") <*>
                     (o .:? "response")
                     <*> (o .:? "name")
                     <*> (o .:? "metadata"))

instance ToJSON Operation where
        toJSON Operation'{..}
          = object
              (catMaybes
                 [("done" .=) <$> _oDone, ("error" .=) <$> _oError,
                  ("response" .=) <$> _oResponse,
                  ("name" .=) <$> _oName,
                  ("metadata" .=) <$> _oMetadata])

-- | A generic empty message that you can re-use to avoid defining duplicated
-- empty messages in your APIs. A typical example is to use it as the
-- request or the response type of an API method. For instance: service Foo
-- { rpc Bar(google.protobuf.Empty) returns (google.protobuf.Empty); } The
-- JSON representation for \`Empty\` is empty JSON object \`{}\`.
--
-- /See:/ 'empty' smart constructor.
data Empty =
    Empty' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Empty' with the minimum fields required to make a request.
--
empty
    :: Empty
empty = Empty'

instance FromJSON Empty where
        parseJSON = withObject "Empty" (\ o -> pure Empty')

instance ToJSON Empty where
        toJSON = const emptyObject

-- | Alternative hypotheses (a.k.a. n-best list).
--
-- /See:/ 'speechRecognitionAlternative' smart constructor.
data SpeechRecognitionAlternative = SpeechRecognitionAlternative'
    { _sraConfidence :: !(Maybe (Textual Double))
    , _sraTranscript :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SpeechRecognitionAlternative' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sraConfidence'
--
-- * 'sraTranscript'
speechRecognitionAlternative
    :: SpeechRecognitionAlternative
speechRecognitionAlternative = 
    SpeechRecognitionAlternative'
    { _sraConfidence = Nothing
    , _sraTranscript = Nothing
    }

-- | [Output-only] The confidence estimate between 0.0 and 1.0. A higher
-- number means the system is more confident that the recognition is
-- correct. This field is typically provided only for the top hypothesis,
-- and only for \`is_final=true\` results. The default of 0.0 is a sentinel
-- value indicating confidence was not set.
sraConfidence :: Lens' SpeechRecognitionAlternative (Maybe Double)
sraConfidence
  = lens _sraConfidence
      (\ s a -> s{_sraConfidence = a})
      . mapping _Coerce

-- | [Output-only] Transcript text representing the words that the user
-- spoke.
sraTranscript :: Lens' SpeechRecognitionAlternative (Maybe Text)
sraTranscript
  = lens _sraTranscript
      (\ s a -> s{_sraTranscript = a})

instance FromJSON SpeechRecognitionAlternative where
        parseJSON
          = withObject "SpeechRecognitionAlternative"
              (\ o ->
                 SpeechRecognitionAlternative' <$>
                   (o .:? "confidence") <*> (o .:? "transcript"))

instance ToJSON SpeechRecognitionAlternative where
        toJSON SpeechRecognitionAlternative'{..}
          = object
              (catMaybes
                 [("confidence" .=) <$> _sraConfidence,
                  ("transcript" .=) <$> _sraTranscript])

--
-- /See:/ 'statusDetailsItem' smart constructor.
newtype StatusDetailsItem = StatusDetailsItem'
    { _sdiAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StatusDetailsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiAddtional'
statusDetailsItem
    :: HashMap Text JSONValue -- ^ 'sdiAddtional'
    -> StatusDetailsItem
statusDetailsItem pSdiAddtional_ = 
    StatusDetailsItem'
    { _sdiAddtional = _Coerce # pSdiAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
sdiAddtional :: Lens' StatusDetailsItem (HashMap Text JSONValue)
sdiAddtional
  = lens _sdiAddtional (\ s a -> s{_sdiAddtional = a})
      . _Coerce

instance FromJSON StatusDetailsItem where
        parseJSON
          = withObject "StatusDetailsItem"
              (\ o -> StatusDetailsItem' <$> (parseJSONObject o))

instance ToJSON StatusDetailsItem where
        toJSON = toJSON . _sdiAddtional

-- | A speech recognition result corresponding to a portion of the audio.
--
-- /See:/ 'speechRecognitionResult' smart constructor.
newtype SpeechRecognitionResult = SpeechRecognitionResult'
    { _srrAlternatives :: Maybe [SpeechRecognitionAlternative]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SpeechRecognitionResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrAlternatives'
speechRecognitionResult
    :: SpeechRecognitionResult
speechRecognitionResult = 
    SpeechRecognitionResult'
    { _srrAlternatives = Nothing
    }

-- | [Output-only] May contain one or more recognition hypotheses (up to the
-- maximum specified in \`max_alternatives\`).
srrAlternatives :: Lens' SpeechRecognitionResult [SpeechRecognitionAlternative]
srrAlternatives
  = lens _srrAlternatives
      (\ s a -> s{_srrAlternatives = a})
      . _Default
      . _Coerce

instance FromJSON SpeechRecognitionResult where
        parseJSON
          = withObject "SpeechRecognitionResult"
              (\ o ->
                 SpeechRecognitionResult' <$>
                   (o .:? "alternatives" .!= mempty))

instance ToJSON SpeechRecognitionResult where
        toJSON SpeechRecognitionResult'{..}
          = object
              (catMaybes
                 [("alternatives" .=) <$> _srrAlternatives])

-- | Contains audio data in the encoding specified in the
-- \`RecognitionConfig\`. Either \`content\` or \`uri\` must be supplied.
-- Supplying both or neither returns google.rpc.Code.INVALID_ARGUMENT. See
-- [audio limits](https:\/\/cloud.google.com\/speech\/limits#content).
--
-- /See:/ 'recognitionAudio' smart constructor.
data RecognitionAudio = RecognitionAudio'
    { _raURI :: !(Maybe Text)
    , _raContent :: !(Maybe Base64)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecognitionAudio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raURI'
--
-- * 'raContent'
recognitionAudio
    :: RecognitionAudio
recognitionAudio = 
    RecognitionAudio'
    { _raURI = Nothing
    , _raContent = Nothing
    }

-- | URI that points to a file that contains audio data bytes as specified in
-- \`RecognitionConfig\`. Currently, only Google Cloud Storage URIs are
-- supported, which must be specified in the following format:
-- \`gs:\/\/bucket_name\/object_name\` (other URI formats return
-- google.rpc.Code.INVALID_ARGUMENT). For more information, see [Request
-- URIs](https:\/\/cloud.google.com\/storage\/docs\/reference-uris).
raURI :: Lens' RecognitionAudio (Maybe Text)
raURI = lens _raURI (\ s a -> s{_raURI = a})

-- | The audio data bytes encoded as specified in \`RecognitionConfig\`.
-- Note: as with all bytes fields, protobuffers use a pure binary
-- representation, whereas JSON representations use base64.
raContent :: Lens' RecognitionAudio (Maybe ByteString)
raContent
  = lens _raContent (\ s a -> s{_raContent = a}) .
      mapping _Base64

instance FromJSON RecognitionAudio where
        parseJSON
          = withObject "RecognitionAudio"
              (\ o ->
                 RecognitionAudio' <$>
                   (o .:? "uri") <*> (o .:? "content"))

instance ToJSON RecognitionAudio where
        toJSON RecognitionAudio'{..}
          = object
              (catMaybes
                 [("uri" .=) <$> _raURI,
                  ("content" .=) <$> _raContent])

-- | \`SyncRecognizeRequest\` is the top-level message sent by the client for
-- the \`SyncRecognize\` method.
--
-- /See:/ 'syncRecognizeRequest' smart constructor.
data SyncRecognizeRequest = SyncRecognizeRequest'
    { _srrConfig :: !(Maybe RecognitionConfig)
    , _srrAudio :: !(Maybe RecognitionAudio)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SyncRecognizeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrConfig'
--
-- * 'srrAudio'
syncRecognizeRequest
    :: SyncRecognizeRequest
syncRecognizeRequest = 
    SyncRecognizeRequest'
    { _srrConfig = Nothing
    , _srrAudio = Nothing
    }

-- | [Required] The \`config\` message provides information to the recognizer
-- that specifies how to process the request.
srrConfig :: Lens' SyncRecognizeRequest (Maybe RecognitionConfig)
srrConfig
  = lens _srrConfig (\ s a -> s{_srrConfig = a})

-- | [Required] The audio data to be recognized.
srrAudio :: Lens' SyncRecognizeRequest (Maybe RecognitionAudio)
srrAudio = lens _srrAudio (\ s a -> s{_srrAudio = a})

instance FromJSON SyncRecognizeRequest where
        parseJSON
          = withObject "SyncRecognizeRequest"
              (\ o ->
                 SyncRecognizeRequest' <$>
                   (o .:? "config") <*> (o .:? "audio"))

instance ToJSON SyncRecognizeRequest where
        toJSON SyncRecognizeRequest'{..}
          = object
              (catMaybes
                 [("config" .=) <$> _srrConfig,
                  ("audio" .=) <$> _srrAudio])

-- | The \`RecognitionConfig\` message provides information to the recognizer
-- that specifies how to process the request.
--
-- /See:/ 'recognitionConfig' smart constructor.
data RecognitionConfig = RecognitionConfig'
    { _rcLanguageCode :: !(Maybe Text)
    , _rcSpeechContext :: !(Maybe SpeechContext)
    , _rcMaxAlternatives :: !(Maybe (Textual Int32))
    , _rcSampleRate :: !(Maybe (Textual Int32))
    , _rcProfanityFilter :: !(Maybe Bool)
    , _rcEncoding :: !(Maybe RecognitionConfigEncoding)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecognitionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcLanguageCode'
--
-- * 'rcSpeechContext'
--
-- * 'rcMaxAlternatives'
--
-- * 'rcSampleRate'
--
-- * 'rcProfanityFilter'
--
-- * 'rcEncoding'
recognitionConfig
    :: RecognitionConfig
recognitionConfig = 
    RecognitionConfig'
    { _rcLanguageCode = Nothing
    , _rcSpeechContext = Nothing
    , _rcMaxAlternatives = Nothing
    , _rcSampleRate = Nothing
    , _rcProfanityFilter = Nothing
    , _rcEncoding = Nothing
    }

-- | [Optional] The language of the supplied audio as a BCP-47 language tag.
-- Example: \"en-GB\" https:\/\/www.rfc-editor.org\/rfc\/bcp\/bcp47.txt If
-- omitted, defaults to \"en-US\". See [Language
-- Support](https:\/\/cloud.google.com\/speech\/docs\/languages) for a list
-- of the currently supported language codes.
rcLanguageCode :: Lens' RecognitionConfig (Maybe Text)
rcLanguageCode
  = lens _rcLanguageCode
      (\ s a -> s{_rcLanguageCode = a})

-- | [Optional] A means to provide context to assist the speech recognition.
rcSpeechContext :: Lens' RecognitionConfig (Maybe SpeechContext)
rcSpeechContext
  = lens _rcSpeechContext
      (\ s a -> s{_rcSpeechContext = a})

-- | [Optional] Maximum number of recognition hypotheses to be returned.
-- Specifically, the maximum number of \`SpeechRecognitionAlternative\`
-- messages within each \`SpeechRecognitionResult\`. The server may return
-- fewer than \`max_alternatives\`. Valid values are \`0\`-\`30\`. A value
-- of \`0\` or \`1\` will return a maximum of \`1\`. If omitted, defaults
-- to \`1\`.
rcMaxAlternatives :: Lens' RecognitionConfig (Maybe Int32)
rcMaxAlternatives
  = lens _rcMaxAlternatives
      (\ s a -> s{_rcMaxAlternatives = a})
      . mapping _Coerce

-- | [Required] Sample rate in Hertz of the audio data sent in all
-- \`RecognitionAudio\` messages. Valid values are: 8000-48000. 16000 is
-- optimal. For best results, set the sampling rate of the audio source to
-- 16000 Hz. If that\'s not possible, use the native sample rate of the
-- audio source (instead of re-sampling).
rcSampleRate :: Lens' RecognitionConfig (Maybe Int32)
rcSampleRate
  = lens _rcSampleRate (\ s a -> s{_rcSampleRate = a})
      . mapping _Coerce

-- | [Optional] If set to \`true\`, the server will attempt to filter out
-- profanities, replacing all but the initial character in each filtered
-- word with asterisks, e.g. \"f***\". If set to \`false\` or omitted,
-- profanities won\'t be filtered out.
rcProfanityFilter :: Lens' RecognitionConfig (Maybe Bool)
rcProfanityFilter
  = lens _rcProfanityFilter
      (\ s a -> s{_rcProfanityFilter = a})

-- | [Required] Encoding of audio data sent in all \`RecognitionAudio\`
-- messages.
rcEncoding :: Lens' RecognitionConfig (Maybe RecognitionConfigEncoding)
rcEncoding
  = lens _rcEncoding (\ s a -> s{_rcEncoding = a})

instance FromJSON RecognitionConfig where
        parseJSON
          = withObject "RecognitionConfig"
              (\ o ->
                 RecognitionConfig' <$>
                   (o .:? "languageCode") <*> (o .:? "speechContext")
                     <*> (o .:? "maxAlternatives")
                     <*> (o .:? "sampleRate")
                     <*> (o .:? "profanityFilter")
                     <*> (o .:? "encoding"))

instance ToJSON RecognitionConfig where
        toJSON RecognitionConfig'{..}
          = object
              (catMaybes
                 [("languageCode" .=) <$> _rcLanguageCode,
                  ("speechContext" .=) <$> _rcSpeechContext,
                  ("maxAlternatives" .=) <$> _rcMaxAlternatives,
                  ("sampleRate" .=) <$> _rcSampleRate,
                  ("profanityFilter" .=) <$> _rcProfanityFilter,
                  ("encoding" .=) <$> _rcEncoding])

-- | \`SyncRecognizeResponse\` is the only message returned to the client by
-- \`SyncRecognize\`. It contains the result as zero or more sequential
-- \`SpeechRecognitionResult\` messages.
--
-- /See:/ 'syncRecognizeResponse' smart constructor.
newtype SyncRecognizeResponse = SyncRecognizeResponse'
    { _srrResults :: Maybe [SpeechRecognitionResult]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SyncRecognizeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrResults'
syncRecognizeResponse
    :: SyncRecognizeResponse
syncRecognizeResponse = 
    SyncRecognizeResponse'
    { _srrResults = Nothing
    }

-- | [Output-only] Sequential list of transcription results corresponding to
-- sequential portions of audio.
srrResults :: Lens' SyncRecognizeResponse [SpeechRecognitionResult]
srrResults
  = lens _srrResults (\ s a -> s{_srrResults = a}) .
      _Default
      . _Coerce

instance FromJSON SyncRecognizeResponse where
        parseJSON
          = withObject "SyncRecognizeResponse"
              (\ o ->
                 SyncRecognizeResponse' <$>
                   (o .:? "results" .!= mempty))

instance ToJSON SyncRecognizeResponse where
        toJSON SyncRecognizeResponse'{..}
          = object (catMaybes [("results" .=) <$> _srrResults])

-- | Service-specific metadata associated with the operation. It typically
-- contains progress information and common metadata such as create time.
-- Some services might not provide such metadata. Any method that returns a
-- long-running operation should document the metadata type, if any.
--
-- /See:/ 'operationMetadata' smart constructor.
newtype OperationMetadata = OperationMetadata'
    { _omAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'omAddtional'
operationMetadata
    :: HashMap Text JSONValue -- ^ 'omAddtional'
    -> OperationMetadata
operationMetadata pOmAddtional_ = 
    OperationMetadata'
    { _omAddtional = _Coerce # pOmAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
omAddtional :: Lens' OperationMetadata (HashMap Text JSONValue)
omAddtional
  = lens _omAddtional (\ s a -> s{_omAddtional = a}) .
      _Coerce

instance FromJSON OperationMetadata where
        parseJSON
          = withObject "OperationMetadata"
              (\ o -> OperationMetadata' <$> (parseJSONObject o))

instance ToJSON OperationMetadata where
        toJSON = toJSON . _omAddtional

-- | \`AsyncRecognizeRequest\` is the top-level message sent by the client
-- for the \`AsyncRecognize\` method.
--
-- /See:/ 'asyncRecognizeRequest' smart constructor.
data AsyncRecognizeRequest = AsyncRecognizeRequest'
    { _arrConfig :: !(Maybe RecognitionConfig)
    , _arrAudio :: !(Maybe RecognitionAudio)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AsyncRecognizeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arrConfig'
--
-- * 'arrAudio'
asyncRecognizeRequest
    :: AsyncRecognizeRequest
asyncRecognizeRequest = 
    AsyncRecognizeRequest'
    { _arrConfig = Nothing
    , _arrAudio = Nothing
    }

-- | [Required] The \`config\` message provides information to the recognizer
-- that specifies how to process the request.
arrConfig :: Lens' AsyncRecognizeRequest (Maybe RecognitionConfig)
arrConfig
  = lens _arrConfig (\ s a -> s{_arrConfig = a})

-- | [Required] The audio data to be recognized.
arrAudio :: Lens' AsyncRecognizeRequest (Maybe RecognitionAudio)
arrAudio = lens _arrAudio (\ s a -> s{_arrAudio = a})

instance FromJSON AsyncRecognizeRequest where
        parseJSON
          = withObject "AsyncRecognizeRequest"
              (\ o ->
                 AsyncRecognizeRequest' <$>
                   (o .:? "config") <*> (o .:? "audio"))

instance ToJSON AsyncRecognizeRequest where
        toJSON AsyncRecognizeRequest'{..}
          = object
              (catMaybes
                 [("config" .=) <$> _arrConfig,
                  ("audio" .=) <$> _arrAudio])

-- | The normal response of the operation in case of success. If the original
-- method returns no data on success, such as \`Delete\`, the response is
-- \`google.protobuf.Empty\`. If the original method is standard
-- \`Get\`\/\`Create\`\/\`Update\`, the response should be the resource.
-- For other methods, the response should have the type \`XxxResponse\`,
-- where \`Xxx\` is the original method name. For example, if the original
-- method name is \`TakeSnapshot()\`, the inferred response type is
-- \`TakeSnapshotResponse\`.
--
-- /See:/ 'operationResponse' smart constructor.
newtype OperationResponse = OperationResponse'
    { _orAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orAddtional'
operationResponse
    :: HashMap Text JSONValue -- ^ 'orAddtional'
    -> OperationResponse
operationResponse pOrAddtional_ = 
    OperationResponse'
    { _orAddtional = _Coerce # pOrAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
orAddtional :: Lens' OperationResponse (HashMap Text JSONValue)
orAddtional
  = lens _orAddtional (\ s a -> s{_orAddtional = a}) .
      _Coerce

instance FromJSON OperationResponse where
        parseJSON
          = withObject "OperationResponse"
              (\ o -> OperationResponse' <$> (parseJSONObject o))

instance ToJSON OperationResponse where
        toJSON = toJSON . _orAddtional
