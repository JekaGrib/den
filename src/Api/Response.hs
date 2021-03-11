{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Response where

import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                      as T
import           Control.Applicative


data Answer 
    = Answer       { ts       :: T.Text,
                     updates  :: [Update] }
    | FailAnswer   { fail'    :: Int }               
    | FailTSAnswer { fail''   :: Int,
                     ts''     :: Int }  
    | ErrorAnswer  { error' :: Object } deriving (Generic, Show)
         
instance FromJSON Answer where
    parseJSON (Object v) = (Answer 
        <$> v .: "ts"
        <*> v .: "updates") <|> ( FailAnswer
        <$> v .: "fail") <|> ( FailTSAnswer
        <$> v .: "fail"
        <*> v .: "ts") <|> ( ErrorAnswer
        <$> v .: "error")



data Update 
  = Update {typeUpd :: T.Text,
            objectUpd  :: AboutObj} 
  | UnknownUpdate  {typeUpdate :: T.Text} deriving (Generic, Show)

instance FromJSON Update where
    parseJSON (Object v) = (Update
        <$> v .: "type"
        <*> v .: "object") <|> (UnknownUpdate 
        <$> v .: "type")


data AboutObj = AboutObj {
      from_id  :: Int
    , id  :: Int
    , peer_id  :: Int
    , text  :: T.Text
    , fwd_messages :: [Int]
    , attachments :: [Attachment]
    } deriving (Generic, Show)

instance FromJSON AboutObj

data Attachment 
    = PhotoAttachment 
      { type' :: T.Text
      , photo :: Photo }
     deriving (Generic, Show)

instance FromJSON Attachment where
    parseJSON (Object v) = (PhotoAttachment
        <$> v .: "type"
        <*> v .: "photo") 




data Photo = Photo {
      size :: [Size]
    } deriving (Generic, Show)

instance FromJSON Photo

data Size = Size {
      height :: Integer
    , width :: Integer
    , url :: T.Text
    } deriving (Generic, Show)

instance FromJSON Size

data GetPollServerJSONBody 
    = GetPollServerJSONBody { response :: ServerInfo} 
    | ErrorAnswerServ  { error'' :: Object } deriving (Generic, Show)

instance FromJSON GetPollServerJSONBody where
    parseJSON (Object v) = (GetPollServerJSONBody
        <$> v .: "response") <|> ( ErrorAnswerServ
        <$> v .: "error")


data ServerInfo 
    = ServerInfo { key :: T.Text,
                   server  :: T.Text,
                   tsSI  :: T.Text} deriving (Generic, Show)


instance FromJSON ServerInfo where
    parseJSON (Object v) = ServerInfo
        <$> v .: "key"
        <*> v .: "server"
        <*> v .: "ts" 

data Response 
    = Response { response' :: Int }
    | ErrorAnswerMsg  { error''' :: Object } deriving (Generic, Show)

instance FromJSON Response where
    parseJSON (Object v) = (Response
        <$> v .: "response") <|> ( ErrorAnswerMsg
        <$> v .: "error")

data ErrorInfo = ErrorInfo { error_code :: Int} deriving (Generic, Show)

instance FromJSON ErrorInfo

data PhotoServerResponse = PhotoServerResponse {upload_url :: T.Text} deriving (Generic, Show)

instance FromJSON PhotoServerResponse
