{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Speech.Types.Sum
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Speech.Types.Sum where

import Network.Google.Prelude

-- | V1 error format.
data Xgafv
    = X1
      -- ^ @1@
      -- v1 error format
    | X2
      -- ^ @2@
      -- v2 error format
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable Xgafv

instance FromHttpApiData Xgafv where
    parseQueryParam = \case
        "1" -> Right X1
        "2" -> Right X2
        x -> Left ("Unable to parse Xgafv from: " <> x)

instance ToHttpApiData Xgafv where
    toQueryParam = \case
        X1 -> "1"
        X2 -> "2"

instance FromJSON Xgafv where
    parseJSON = parseJSONText "Xgafv"

instance ToJSON Xgafv where
    toJSON = toJSONText

-- | [Required] Encoding of audio data sent in all \`RecognitionAudio\`
-- messages.
data RecognitionConfigEncoding
    = EncodingUnspecified
      -- ^ @ENCODING_UNSPECIFIED@
      -- Not specified. Will return result google.rpc.Code.INVALID_ARGUMENT.
    | LINEAR16
      -- ^ @LINEAR16@
      -- Uncompressed 16-bit signed little-endian samples (Linear PCM). This is
      -- the only encoding that may be used by \`AsyncRecognize\`.
    | Flac
      -- ^ @FLAC@
      -- This is the recommended encoding for \`SyncRecognize\` and
      -- \`StreamingRecognize\` because it uses lossless compression; therefore
      -- recognition accuracy is not compromised by a lossy codec. The stream
      -- FLAC (Free Lossless Audio Codec) encoding is specified at:
      -- http:\/\/flac.sourceforge.net\/documentation.html. 16-bit and 24-bit
      -- samples are supported. Not all fields in STREAMINFO are supported.
    | Mulaw
      -- ^ @MULAW@
      -- 8-bit samples that compand 14-bit audio samples using G.711
      -- PCMU\/mu-law.
    | Amr
      -- ^ @AMR@
      -- Adaptive Multi-Rate Narrowband codec. \`sample_rate\` must be 8000 Hz.
    | AmrWb
      -- ^ @AMR_WB@
      -- Adaptive Multi-Rate Wideband codec. \`sample_rate\` must be 16000 Hz.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable RecognitionConfigEncoding

instance FromHttpApiData RecognitionConfigEncoding where
    parseQueryParam = \case
        "ENCODING_UNSPECIFIED" -> Right EncodingUnspecified
        "LINEAR16" -> Right LINEAR16
        "FLAC" -> Right Flac
        "MULAW" -> Right Mulaw
        "AMR" -> Right Amr
        "AMR_WB" -> Right AmrWb
        x -> Left ("Unable to parse RecognitionConfigEncoding from: " <> x)

instance ToHttpApiData RecognitionConfigEncoding where
    toQueryParam = \case
        EncodingUnspecified -> "ENCODING_UNSPECIFIED"
        LINEAR16 -> "LINEAR16"
        Flac -> "FLAC"
        Mulaw -> "MULAW"
        Amr -> "AMR"
        AmrWb -> "AMR_WB"

instance FromJSON RecognitionConfigEncoding where
    parseJSON = parseJSONText "RecognitionConfigEncoding"

instance ToJSON RecognitionConfigEncoding where
    toJSON = toJSONText
