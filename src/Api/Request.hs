{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Request where

import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                      as T

kB = KeyBoard True [[button1],[button2],[button3],[button4],[button5]] False

button1 = Button action1 "positive"
button2 = Button action2 "positive"
button3 = Button action3 "positive"
button4 = Button action4 "positive"
button5 = Button action5 "positive"

action1 = Action "text" "1"
action2 = Action "text" "2"
action3 = Action "text" "3"
action4 = Action "text" "4"
action5 = Action "text" "5"

data KeyBoard = KeyBoard
  { one_time  :: Bool,
    buttons   :: [[Button]],
    inline    :: Bool
    } deriving (Generic, Show)

instance ToJSON KeyBoard where
    toEncoding = genericToEncoding defaultOptions

data Button = Button
  { action  :: Action,
    color   :: T.Text
    } deriving (Generic, Show)

instance ToJSON Button where
    toEncoding = genericToEncoding defaultOptions

data Action = Action
  { typeA  :: T.Text,
    label  :: T.Text
    } deriving (Generic, Show)

instance ToJSON Action where
  toJSON (Action typeA  label ) =
    object ["type" .= typeA , "label" .= label]
  toEncoding (Action typeA  label ) =
    pairs ("type" .= typeA  <> "label" .= label)

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
