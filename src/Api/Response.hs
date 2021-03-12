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
            objectUpd  :: AboutObj
            } deriving ( Show)

instance FromJSON Update where
    parseJSON (Object v) = Update
        <$> v .: "type"
        <*> v .: "object"


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
      , photoPA :: Photo }
     deriving (Generic, Show)

instance FromJSON Attachment where
    parseJSON (Object v) = (PhotoAttachment
        <$> v .: "type"
        <*> v .: "photo") 




data Photo = Photo {
      sizes :: [Size]
    } deriving (Generic, Show)

instance FromJSON Photo

data Size = Size {
      height :: Integer
    , width :: Integer
    , url :: T.Text
    } deriving (Generic, Show)

instance FromJSON Size

data LoadPhotoResp = LoadPhotoResp {
      server :: Int
    , hash :: String
    , photo :: String
    } deriving (Generic, Show)

instance FromJSON LoadPhotoResp

data SavePhotoResp = SavePhotoResp {responseSPR :: [PhotoInfo]} deriving (Generic, Show)

instance FromJSON SavePhotoResp where
    parseJSON (Object v) = SavePhotoResp
        <$> v .: "response"

data PhotoInfo = PhotoInfo {
      idSPR :: Int
    , owner_id :: Int
    , access_key :: String
    } deriving (Generic, Show)

instance FromJSON PhotoInfo where
      parseJSON (Object v) = PhotoInfo
        <$> v .: "id"
        <*> v .: "owner_id"
        <*> v .: "access_key"


data GetPollServerJSONBody 
    = GetPollServerJSONBody { response :: ServerInfo} 
    | ErrorAnswerServ  { error'' :: Object } deriving (Generic, Show)

instance FromJSON GetPollServerJSONBody where
    parseJSON (Object v) = (GetPollServerJSONBody
        <$> v .: "response") <|> ( ErrorAnswerServ
        <$> v .: "error")


data ServerInfo 
    = ServerInfo { key :: T.Text,
                   serverSI  :: T.Text,
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

data PhotoServerResponse = PhotoServerResponse {responsePSR :: UploadUrl} deriving (Generic, Show)

instance FromJSON PhotoServerResponse where
    parseJSON (Object v) = PhotoServerResponse
        <$> v .: "response"

data UploadUrl = UploadUrl {upload_url :: T.Text} deriving (Generic, Show)

instance FromJSON UploadUrl