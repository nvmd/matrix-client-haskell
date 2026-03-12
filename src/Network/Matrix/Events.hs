{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Matrix event data type
module Network.Matrix.Events (
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
import Network.Matrix.Messages (RoomMessage(..))

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
                [ "m.relates_to"
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
