{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Request where

import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                      as T



{-data JSONBodyOffset = JSONBodyOffset {
      offset :: Int
    } deriving (Generic, Show)

instance ToJSON JSONBodyOffset where
    toEncoding = genericToEncoding defaultOptions


data JSONBodyTimeOut = JSONBodyTimeOut {
      timeout :: Int
    } deriving (Generic, Show)

instance ToJSON JSONBodyTimeOut where
    toEncoding = genericToEncoding defaultOptions


data SendMsgJSONBody = SendMsgJSONBody {
      chat_id :: Int
    , text  :: T.Text
    } deriving (Generic, Show)

instance ToJSON SendMsgJSONBody where
    toEncoding = genericToEncoding defaultOptions

data CopyMsgJSONBody = CopyMsgJSONBody {
      chat_idCM :: Int
    , from_chat_idCM :: Int
    , msg_idCM  :: Int
    } deriving (Generic, Show)

instance ToJSON CopyMsgJSONBody where
  toJSON (CopyMsgJSONBody chat_idCM  from_chat_idCM msg_idCM) =
    object ["chat_id" .= chat_idCM , "from_chat_id" .= from_chat_idCM, "message_id" .= msg_idCM]
  toEncoding (CopyMsgJSONBody chat_idCM  from_chat_idCM msg_idCM) =
    pairs ("chat_id" .= chat_idCM  <> "from_chat_id" .= from_chat_idCM <> "message_id" .= msg_idCM)

data KeybJSONBody = KeybJSONBody {
      chat_idKeyb :: Int
    , textKeyb  :: T.Text
    , reply_markup :: KeyBoard
    } deriving (Generic, Show)

instance ToJSON KeybJSONBody where
  toJSON (KeybJSONBody chat_idKeyb  textKeyb reply_markup) =
    object ["chat_id" .= chat_idKeyb , "text" .= textKeyb, "reply_markup" .= reply_markup]
  toEncoding (KeybJSONBody chat_idKeyb  textKeyb reply_markup) =
    pairs ("chat_id" .= chat_idKeyb  <> "text" .= textKeyb <> "reply_markup" .= reply_markup)


data KeyBoard = KeyBoard {
      keyboard :: [[KeyButton]]
    , one_time_keyboard  :: Bool
    } deriving (Generic, Show)

instance ToJSON KeyBoard where
  toJSON (KeyBoard keyboard one_time_keyboard) =
    object ["keyboard" .= keyboard, "one_time_keyboard" .= one_time_keyboard]
  toEncoding (KeyBoard keyboard one_time_keyboard) =
    pairs ("keyboard" .= keyboard <> "one_time_keyboard" .= one_time_keyboard)    


data KeyButton = KeyButton {
      textBtn  :: T.Text
    } deriving (Generic, Show)

instance ToJSON KeyButton where
  toJSON (KeyButton textBtn) =
    object ["text" .= textBtn]
  toEncoding (KeyButton textBtn) =
    pairs ("text" .= textBtn)
-}
