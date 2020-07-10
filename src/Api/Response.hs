{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Response where

import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                      as T


data FromGetUpdatesJSON = FromGetUpdatesJSON {
      ts :: T.Text
    , updates  :: [Update]
    } deriving (Generic, Show)

instance FromJSON FromGetUpdatesJSON where
    parseJSON = withObject "FromGetUpdatesJSON" $ \v -> FromGetUpdatesJSON
        <$> v .: "ts"
        <*> v .: "updates"


data Update = Update {
      typeUpd :: T.Text
    , objectUpd  :: AboutObj
    , group_id  :: Int
    , event_id  :: T.Text
    } deriving (Generic, Show)

instance FromJSON Update where
    parseJSON = withObject "Update" $ \v -> Update
        <$> v .: "type"
        <*> v .: "object"
        <*> v .: "group_id"
        <*> v .: "event_id"


data AboutObj = AboutObj {
      date :: Int
    , from_id  :: Int
    , id  :: Int
    , out  :: Int
    , peer_id  :: Int
    , text  :: T.Text
    , conversation_message_id :: Int
    , fwd_messages :: [Int]
    , important :: Bool
    , random_id :: Int
    , attachments :: [Int]
    , is_hidden :: Bool
    } deriving (Generic, Show)

instance FromJSON AboutObj 

