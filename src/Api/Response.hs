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
            } 
    | UnknownUpdate Object 
       deriving ( Show)

instance FromJSON Update where
    parseJSON  (Object v) = (Update
        <$> v .: "type"
        <*> v .: "object") <|> (UnknownUpdate  <$> parseJSON (Object v))


data AboutObj = AboutObj {
      from_id  :: Int
    , id  :: Int
    , peer_id  :: Int
    , text  :: T.Text
    , fwd_messages :: [Object]
    , attachments :: [Attachment]
    , geo :: Maybe Geo
    } deriving (Generic, Show)

instance FromJSON AboutObj

data Attachment 
    = PhotoAttachment 
      { type' :: T.Text
      , photoPA :: Photo }
    | DocAttachment
      { type' :: T.Text
      , docDA :: Doc }
    | AudioMesAttachment
      { type' :: T.Text
      , audio_message :: Audio }
    | VideoAttachment
      { type' :: T.Text
      , docVA :: DocInfo }
    | StickerAttachment
      { type' :: T.Text
      , sticker :: StickerInfo }
    | AudioAttachment
      { type' :: T.Text
      , audio :: DocInfo }
    | MarketAttachment
      { type' :: T.Text
      , market :: DocInfo }
    | WallAttachment
      { type' :: T.Text
      , wall :: WallInfo }
    | PollAttachment
      { type' :: T.Text
      , poll :: DocInfo }
    | UnknownAttachment Object 
     deriving (Generic, Show)

instance FromJSON Attachment where
    parseJSON (Object v) = (PhotoAttachment
        <$> v .: "type"
        <*> v .: "photo") <|> (DocAttachment
        <$> v .: "type"
        <*> v .: "doc") <|> (AudioMesAttachment
        <$> v .: "type"
        <*> v .: "audio_message") <|> (VideoAttachment
        <$> v .: "type"
        <*> v .: "video") <|> (StickerAttachment
        <$> v .: "type"
        <*> v .: "sticker") <|> (AudioAttachment
        <$> v .: "type"
        <*> v .: "audio") <|> (MarketAttachment
        <$> v .: "type"
        <*> v .: "market") <|> (WallAttachment
        <$> v .: "type"
        <*> v .: "wall") <|> (PollAttachment
        <$> v .: "type"
        <*> v .: "poll") <|> (UnknownAttachment  <$> parseJSON (Object v))

data Doc 
    = Doc{
        urlD   :: T.Text
       , ext   :: String
       , title :: String
    } deriving (Show)

instance FromJSON Doc where
    parseJSON (Object v) = Doc
        <$> v .: "url"
        <*> v .: "ext"
        <*> v .: "title"

data Audio = Audio {
      link_ogg :: T.Text
    } deriving (Generic, Show)

instance FromJSON Audio

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

data LoadDocResp = LoadDocResp {
      file :: String
    } deriving (Generic, Show)

instance FromJSON LoadDocResp

data LoadPhotoResp = LoadPhotoResp {
      server :: Int
    , hash :: String
    , photo :: String
    } deriving (Generic, Show)

instance FromJSON LoadPhotoResp

data SavePhotoResp = SavePhotoResp {responseSPR :: [DocInfo]} deriving (Generic, Show)

instance FromJSON SavePhotoResp where
    parseJSON (Object v) = SavePhotoResp
        <$> v .: "response"

data PhotoInfo = PhotoInfo {
      idPI :: Int
    , owner_id :: Int
    , access_key :: String
    } deriving (Generic, Show)

instance FromJSON PhotoInfo where
      parseJSON (Object v) = PhotoInfo
        <$> v .: "id"
        <*> v .: "owner_id"
        <*> v .: "access_key"

data AudioMesInfo = AudioMesInfo {
      idAMI :: Int
    , owner_idAMI :: Int
    , access_keyAMI :: String
    } deriving (Generic, Show)

instance FromJSON AudioMesInfo where
      parseJSON (Object v) = AudioMesInfo
        <$> v .: "id"
        <*> v .: "owner_id"
        <*> v .: "access_key"

data StickerInfo = StickerInfo {
      sticker_id :: Int
    } deriving (Generic, Show)

instance FromJSON StickerInfo

data SaveDocResp = SaveDocResp {responseSDR :: ResponseSDR} deriving (Generic, Show)

instance FromJSON SaveDocResp where
    parseJSON (Object v) = SaveDocResp
        <$> v .: "response"

data ResponseSDR = ResponseSDR {
      typeRSDR :: T.Text
    , docRSDR  :: DocInfo
    } deriving (Generic, Show)

instance FromJSON ResponseSDR where
    parseJSON (Object v) = ResponseSDR
        <$> v .: "type"
        <*> v .: "doc"

data DocInfo = DocInfo {
      idDI :: Int
    , owner_idDI :: Int
    } deriving (Generic, Show)

instance FromJSON DocInfo where
      parseJSON (Object v) = DocInfo
        <$> v .: "id"
        <*> v .: "owner_id"

data WallInfo = WallInfo {
      idWI :: Int
    , from_idWI :: Int
    } deriving (Generic, Show)

instance FromJSON WallInfo where
      parseJSON (Object v) = WallInfo
        <$> v .: "id"
        <*> v .: "from_id"

data SaveDocAuMesResp = SaveDocAuMesResp {responseSDAMR :: ResponseSDAMR} deriving (Generic, Show)

instance FromJSON SaveDocAuMesResp where
    parseJSON (Object v) = SaveDocAuMesResp
        <$> v .: "response"

data ResponseSDAMR = ResponseSDAMR {
      typeSDAMR :: T.Text
    , docSDAMR  :: DocInfo
    } deriving (Generic, Show)

instance FromJSON ResponseSDAMR where
    parseJSON (Object v) = ResponseSDAMR
        <$> v .: "type"
        <*> v .: "audio_message"





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

data UploadServerResponse = UploadServerResponse {responsePSR :: UploadUrl} deriving (Generic, Show)

instance FromJSON UploadServerResponse where
    parseJSON (Object v) = UploadServerResponse
        <$> v .: "response"

data UploadUrl = UploadUrl {upload_url :: T.Text} deriving (Generic, Show)

instance FromJSON UploadUrl

data Geo = Geo {
      typeG :: T.Text
    , coordinates  :: Coordinates
    } deriving (Generic, Show)

instance FromJSON Geo where
    parseJSON (Object v) = Geo
        <$> v .: "type"
        <*> v .: "coordinates"

data Coordinates = Coordinates {
      latitude :: Double
    , longitude  :: Double
    } deriving (Generic, Show)

instance FromJSON Coordinates 