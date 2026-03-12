{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Matrix data types of the m.room.message event (msgtype)
-- | https://spec.matrix.org/v1.17/client-server-api/#mroommessage-msgtypes
module Network.Matrix.Messages (
    RoomMessage (..),
    MessageText (..),
    MessageMedia (..),
    MessageImage,
    MessageFile,
    MessageAudio,
    MessageLocation (..),
    MessageVideo,
    ImageInfo (..),
    ThumbnailInfo (..),
    FileInfo (..),
    AudioInfo (..),
    VideoInfo (..),
    LocationInfo (..),
)
where

import GHC.Generics (Generic)
import Network.Matrix.Internal (aesonOptions)
import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (Object, String), (.:), genericParseJSON, genericToEncoding, genericToJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)


-- Messages of the `m.room.message` event with `msgtype` `text`
-- https://spec.matrix.org/v1.17/client-server-api/#mtext
data MessageText = MessageText
    { mtBody :: Text
    , mtFormat :: Maybe Text        -- required if formatted_body is specified
    , mtFormattedBody :: Maybe Text -- required if format is specified
    }
    deriving (Generic, Show, Eq)

instance FromJSON MessageText where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON MessageText where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions


-- | msgtype [m.image](https://spec.matrix.org/v1.17/client-server-api/#mimage)
data MessageMedia a = MessageMedia
    { mmBody :: Text
    , mmFile :: Maybe Text      -- required if the file is encrypted
    , mmFilename :: Maybe Text  -- since v1.10
    , mmFormat :: Maybe Text    -- since v1.10, required if formatted_body is specified
    , mmFormattedBody :: Maybe Text -- since v1.10, required if format is specified
    , mmInfo :: Maybe a         -- media-specific info object
    , mmUrl :: Maybe Text       -- required if the file is unencrypted
    }
    deriving (Generic, Show, Eq)

instance FromJSON a => FromJSON (MessageMedia a) where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON a => ToJSON (MessageMedia a) where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions

-------------------------------------------------------------------------------
-- *Info records for MessageMedia

-- https://spec.matrix.org/v1.17/client-server-api/#mimage_imageinfo
data ImageInfo = ImageInfo
    { iiH :: Maybe Int
    , iiMimetype :: Maybe Text
    , iiSize :: Maybe Int
    , iiThumbnailFile :: Maybe Object -- EncryptedFile, only present if the thumbnail is encrypted
    , iiThumbnailInfo :: Maybe ThumbnailInfo
    , iiThumbnailUrl :: Maybe Text
    , iiW :: Maybe Int
    }
    deriving (Generic, Show, Eq)

instance FromJSON ImageInfo where
    parseJSON = genericParseJSON aesonOptions
 
instance ToJSON ImageInfo where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions

type MessageImage = MessageMedia ImageInfo

-- ThumbnailInfo used by several Info objects (subset of ImageInfo)
data ThumbnailInfo = ThumbnailInfo
    { tiH :: Maybe Int
    , tiMimetype :: Maybe Text
    , tiSize :: Maybe Int
    , tiW :: Maybe Int
    }
    deriving (Generic, Show, Eq)
instance FromJSON ThumbnailInfo where
    parseJSON = genericParseJSON aesonOptions
 
instance ToJSON ThumbnailInfo where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions

-- https://spec.matrix.org/v1.17/client-server-api/#mfile_fileinfo
data FileInfo = FileInfo
    { fiMimetype :: Maybe Text
    , fiSize :: Maybe Int
    , fiThumbnailFile :: Maybe Object   -- EncryptedFile, only present if the thumbnail is encrypted
    , fiThumbnailInfo :: Maybe ThumbnailInfo
    , fiThumbnailUrl :: Maybe Text
    }
    deriving (Generic, Show, Eq)
instance FromJSON FileInfo where
    parseJSON = genericParseJSON aesonOptions
 
instance ToJSON FileInfo where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions
type MessageFile = MessageMedia FileInfo

-- https://spec.matrix.org/v1.17/client-server-api/#maudio_audioinfo
data AudioInfo = AudioInfo
    { aiDuration :: Maybe Int
    , aiMimetype :: Maybe Text
    , aiSize :: Maybe Int
    }
    deriving (Generic, Show, Eq)
instance FromJSON AudioInfo where
    parseJSON = genericParseJSON aesonOptions
 
instance ToJSON AudioInfo where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions

type MessageAudio = MessageMedia AudioInfo

-- https://spec.matrix.org/v1.17/client-server-api/#mvideo_videoinfo
data VideoInfo = VideoInfo
    { viDuration :: Maybe Int
    , viH :: Maybe Int
    , viMimetype :: Maybe Text
    , viSize :: Maybe Int
    , viThumbnailFile :: Maybe Object   -- EncryptedFile, only present if the thumbnail is encrypted
    , viThumbnailInfo :: Maybe ThumbnailInfo
    , viThumbnailUrl :: Maybe Text
    , viW :: Maybe Int
    }
    deriving (Generic, Show, Eq)
instance FromJSON VideoInfo where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON VideoInfo where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions

type MessageVideo = MessageMedia VideoInfo


-- https://spec.matrix.org/v1.17/client-server-api/#mlocation
data MessageLocation = MessageLocation
    { mlBody :: Text   -- EncryptedFile, only present if the thumbnail is encrypted
    , mlGeoUri :: Text
    , mlInfo :: Maybe LocationInfo
    }
    deriving (Generic, Show, Eq)
instance FromJSON MessageLocation where
    parseJSON = genericParseJSON aesonOptions
instance ToJSON   MessageLocation where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions

data LocationInfo = LocationInfo
    { liThumbnailFile :: Maybe Object   -- EncryptedFile, only present if the thumbnail is encrypted
    , liThumbnailInfo :: Maybe ThumbnailInfo
    , liThumbnailUrl :: Maybe Text
    }
    deriving (Generic, Show, Eq)

instance FromJSON LocationInfo where
    parseJSON = genericParseJSON aesonOptions
 
instance ToJSON LocationInfo where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions


-------------------------------------------------------------------------------
-- msgtypes of the `m.room.message` event
-- https://spec.matrix.org/v1.17/client-server-api/#mroommessage-msgtypes
data RoomMessage
    = -- | https://spec.matrix.org/v1.17/client-server-api/#mtext
      RoomMessageText MessageText
    | -- | https://spec.matrix.org/v1.17/client-server-api/#memote
      RoomMessageEmote MessageText
    | -- | https://spec.matrix.org/v1.17/client-server-api/#mnotice
      RoomMessageNotice MessageText
    | -- | https://spec.matrix.org/v1.17/client-server-api/#mimage
      RoomMessageImage MessageImage
    | -- | https://spec.matrix.org/v1.17/client-server-api/#mfile
      RoomMessageFile MessageFile
    | -- | https://spec.matrix.org/v1.17/client-server-api/#maudio
      RoomMessageAudio MessageAudio
    | -- | https://spec.matrix.org/v1.17/client-server-api/#mlocation
      RoomMessageLocation MessageLocation
    | -- | https://spec.matrix.org/v1.17/client-server-api/#mvideo
      RoomMessageVideo MessageVideo
    | RoomMessageUnknown Object
    deriving (Generic, Show, Eq)

instance ToJSON RoomMessage where
    toJSON msg = case msg of
        RoomMessageText mc       -> toTaggedJSON "m.text" mc
        RoomMessageEmote mc      -> toTaggedJSON "m.emote" mc
        RoomMessageNotice mc     -> toTaggedJSON "m.notice" mc
        RoomMessageImage mc      -> toTaggedJSON "m.image" mc
        RoomMessageFile mc       -> toTaggedJSON "m.file" mc
        RoomMessageAudio mc      -> toTaggedJSON "m.audio" mc
        RoomMessageLocation mc   -> toTaggedJSON "m.location" mc
        RoomMessageVideo mc      -> toTaggedJSON "m.video" mc
        RoomMessageUnknown obj   -> Object obj
      where
        toTaggedJSON tag m = mergeTag tag (toJSON m)
        mergeTag tag (Object o) = Object $ KeyMap.insert "msgtype" (String tag) o
        mergeTag _ v = v

instance FromJSON RoomMessage where
    parseJSON msg@(Object content) = parseByMessageType <|> pure (RoomMessageUnknown content)
      where
        parseByMessageType = do
            msgType <- content .: "msgtype"
            case (msgType :: Text) of
                "m.text"     -> RoomMessageText     <$> parseJSON msg
                "m.emote"    -> RoomMessageEmote    <$> parseJSON msg
                "m.notice"   -> RoomMessageNotice   <$> parseJSON msg
                "m.image"    -> RoomMessageImage    <$> parseJSON msg
                "m.file"     -> RoomMessageFile     <$> parseJSON msg
                "m.audio"    -> RoomMessageAudio    <$> parseJSON msg
                "m.location" -> RoomMessageLocation <$> parseJSON msg
                "m.video"    -> RoomMessageVideo    <$> parseJSON msg
                _            -> mzero
    parseJSON _ = mzero
