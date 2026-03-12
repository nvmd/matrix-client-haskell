{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Matrix data types of the m.room.message event (msgtype)
module Network.Matrix.Messages (
    MessageText (..),
    MessageImage (..),
    RoomMessage (..),
)
where

import GHC.Generics (Generic)
import Network.Matrix.Internal (aesonOptions)
import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (Object, String), object, (.:), (.=), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)


-- Messages of the `m.room.message` event with `msgtype` `text`
-- https://spec.matrix.org/v1.17/client-server-api/#mtext
data MessageText = MessageText
    { mtBody :: Text
    , mtFormat :: Maybe Text
    , mtFormattedBody :: Maybe Text
    }
    deriving (Generic, Show, Eq)

instance FromJSON MessageText where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON MessageText where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions

-- | msgtype [m.image](https://spec.matrix.org/v1.17/client-server-api/#mimage)
data MessageImage = MessageImage
    { miBody :: Text
    , miFile :: Maybe Text      -- required if the file is encrypted
    , miFilename :: Maybe Text  -- since v1.10
    , miFormat :: Maybe Text    -- since v1.10, required if formatted_body is specified
    , miFormattedBody :: Maybe Text -- since v1.10, required if format is specified
    -- , miInfo :: Maybe ImageInfo -- https://spec.matrix.org/v1.17/client-server-api/#mimage_imageinfo
    , miUrl :: Maybe Text       -- required if the file is unencrypted
    }
    deriving (Generic, Show, Eq)

instance FromJSON MessageImage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON MessageImage where
    toJSON = genericToJSON aesonOptions
    toEncoding = genericToEncoding aesonOptions

-- msgtypes of the `m.room.message` event
-- https://spec.matrix.org/v1.17/client-server-api/#mroommessage-msgtypes
data RoomMessage
    = -- | https://spec.matrix.org/v1.17/client-server-api/#mtext
      RoomMessageText MessageText
    | -- | https://spec.matrix.org/v1.17/client-server-api/#mimage
      RoomMessageImage MessageImage
    | RoomMessageUnknown Object
    deriving (Generic, Show, Eq)

instance ToJSON RoomMessage where
    toJSON msg = case msg of
        RoomMessageText mt -> mergeTag "m.text" (toJSON mt)
        RoomMessageImage mi -> mergeTag "m.image" (toJSON mi)
        RoomMessageUnknown obj -> Object obj
      where
        mergeTag tag (Object o) = Object $ KeyMap.insert "msgtype" (String tag) o
        mergeTag _ v = v

instance FromJSON RoomMessage where
    parseJSON (Object content) = parseByMessageType <|> pure (RoomMessageUnknown content)
      where
        parseByMessageType = do
            msgType <- content .: "msgtype"
            case (msgType :: Text) of
                "m.text"  -> RoomMessageText  <$> parseJSON (Object content)
                "m.image" -> RoomMessageImage <$> parseJSON (Object content)
                _         -> mzero
    parseJSON _ = mzero
