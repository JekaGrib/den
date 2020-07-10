{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Request where

import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                      as T


data GetPollServerJSONBody = GetPollServerJSONBody {
      response :: PollServerInfo
    } deriving (Generic, Show)

instance FromJSON GetPollServerJSONBody where
    parseJSON = withObject "GetPollServerJSONBody" $ \v -> GetPollServerJSONBody
        <$> v .: "response"


data PollServerInfo = PollServerInfo {
      keyPollServ :: T.Text
    , serverPollServ  :: T.Text
    , tsPollServ  :: T.Text
    } deriving (Generic, Show)

instance FromJSON PollServerInfo where
    parseJSON = withObject "PollServerInfo" $ \v -> PollServerInfo
        <$> v .: "key"
        <*> v .: "server"
        <*> v .: "ts"




