{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Matrix event data type
module Network.Matrix.Events (
    MessageText (..),
    RoomMessage (..),
    Event (..),
    EventID (..),
    Annotation (..),
    eventType,
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

data RelatedMessage = RelatedMessage
    { rmMessage :: RoomMessage
    , rmRelatedTo :: EventID
    }
    deriving (Show, Eq)

data Event
    = -- | [`m.room.message`](https://spec.matrix.org/v1.17/client-server-api/#mroommessage)
      EventRoomMessage RoomMessage
    | -- | A reply defined by the parent event id and the reply message
      EventRoomReply EventID RoomMessage
    | -- | An edit defined by the original message and the new message
      EventRoomEdit (EventID, RoomMessage) RoomMessage
    | -- [`m.reaction`](https://spec.matrix.org/v1.17/client-server-api/#mreaction)
      EventReaction EventID Annotation
    | EventUnknown Object
    deriving (Eq, Show)

instance ToJSON Event where
    toJSON event = case event of
        EventRoomMessage msg -> toJSON msg
        EventRoomReply eventID msg ->
            withObject (KeyMap.insert "m.relates_to"
                (object["m.in_reply_to" .= toJSON eventID])) (toJSON msg)
        EventRoomEdit (EventID eventID, msg) newMsg ->
            withObject
                ( KeyMap.insert "m.relates_to"
                    (object
                        [ "rel_type" .= ("m.replace" :: Text)
                        , "event_id" .= eventID
                        ]
                    )
                . KeyMap.insert "m.new_content" (toJSON newMsg)
                ) (toJSON msg)
        EventReaction (EventID eventID) (Annotation annotationText) ->
            object
                [ "msgtype" .= ("m.reaction" :: Text)
                , "m.relates_to"
                    .= object
                        [ "rel_type" .= ("m.annotation" :: Text)
                        , "event_id" .= eventID
                        , "key" .= annotationText
                        ]
                ]
        EventUnknown v -> Object v
      where
        withObject f (Object o) = Object (f o)
        withObject _ v          = v

instance FromJSON Event where
    parseJSON (Object content) =
        parseRelated <|> parseMessage <|> pure (EventUnknown content)
      where
        parseMessage = EventRoomMessage <$> parseJSON (Object content)
        -- https://spec.matrix.org/v1.17/client-server-api/#forming-relationships-between-events
        parseRelated = do
            relateM <- content .: "m.relates_to"
            case relateM of
                Object relate ->
                    parseReply relate
                        <|> parseByRelType relate
                _ -> mzero
        -- rich replies is a special kind of a relationship not using rel_type
        -- https://spec.matrix.org/v1.17/client-server-api/#rich-replies
        parseReply relate =
            EventRoomReply <$> relate .: "m.in_reply_to" <*> parseJSON (Object content)
        -- relationships using rel_type
        parseByRelType relate = do
            rel_type <- relate .: "rel_type"
            case (rel_type :: Text) of
                -- https://spec.matrix.org/v1.17/client-server-api/#event-replacements
                "m.replace" -> do
                    ev <- EventID <$> relate .: "event_id"
                    msg <- parseJSON (Object content)
                    EventRoomEdit (ev, msg) <$> content .: "m.new_content"
                -- https://spec.matrix.org/v1.17/client-server-api/#mannotation-relationship-type
                "m.annotation" -> do
                    ev <- EventID <$> relate .: "event_id"
                    annotation <- Annotation <$> relate .: "key"
                    pure $ EventReaction ev annotation
                _ -> mzero
    parseJSON _ = mzero

eventType :: Event -> Text
eventType event = case event of
    EventRoomMessage _ -> "m.room.message"
    EventRoomReply _ _ -> "m.room.message"
    EventRoomEdit _ _ -> "m.room.message"
    EventReaction _ _ -> "m.reaction" -- https://spec.matrix.org/latest/client-server-api/#mreaction
    EventUnknown _ -> error $ "Event is not implemented: " <> show event

newtype Annotation = Annotation {unAnnotation :: Text} deriving (Show, Eq, Ord)

newtype EventID = EventID {unEventID :: Text} deriving (Show, Eq, Ord)

instance FromJSON EventID where
    parseJSON (Object v) = EventID <$> v .: "event_id"
    parseJSON _ = mzero

instance ToJSON EventID where
    toJSON (EventID v) = object ["event_id" .= v]
