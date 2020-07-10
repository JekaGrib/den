{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Api.Request
import           Api.Response
import           Network.HTTP.Simple            ( parseRequest, setRequestBody, getResponseBody, httpLBS )
import           Network.HTTP.Client.Conduit               
import qualified Data.ByteString.Lazy           as LBS
import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                      as T
import           Data.Maybe                     ( fromJust )
import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Control.Monad.Catch
import qualified Control.Exception              as E

data Msg           = Msg        T.Text            deriving (Eq,Show)
data ToUserId      = ToUserId   Int               deriving (Eq,Show)

data VKBotException 
  = DuringGetUpdatesException String
  | CheckGetUpdatesResponseException String
  | DuringSendMsgException Msg ToUserId String
  | CheckSendMsgResponseException Msg ToUserId String
  | DuringSendKeybException ToUserId String
  | CheckSendKeybResponseException ToUserId String
  | DuringGetTimeException String
  | DuringPullConfigException String
  | DuringParseConfigException String
    deriving (Eq,Show)

instance Exception VKBotException 


data Handle m = Handle
  { hConf             :: Config,
    getLongPollServer :: m LBS.ByteString,
    getUpdates        :: T.Text -> T.Text -> T.Text -> m LBS.ByteString,
    sendMsg           :: Int -> T.Text -> m LBS.ByteString,
    sendKeyb          :: Int -> Int -> T.Text -> m LBS.ByteString
    }

data Config = Config 
  { cStartN   :: Int,
    cBotToken :: String,
    cHelpMsg  :: String,
    cRepeatQ  :: String,
    cGroupId  :: Int
    }

data OpenRepeat = OpenRepeat Int
                        deriving (Eq,Show)

--keyB = {"one_time": true,"buttons": [[{"action": {"type": "text","label": "1"},"color": "positive"}],[{"action": {"type": "text","label": "2"},"color": "positive"}],[{"action": {"type": "text","label": "3"},"color": "positive"}],[{"action": {"type": "text","label": "4"},"color": "positive"}],[{"action": {"type": "text","label": "5"},"color": "positive"}]],"inline":false}
keyB = "%7B%22one_time%22%3A%20true%2C%22buttons%22%3A%20%5B%5B%7B%22action%22%3A%20%7B%22type%22%3A%20%22text%22%2C%22label%22%3A%20%221%22%7D%2C%22color%22%3A%20%22positive%22%7D%5D%2C%5B%7B%22action%22%3A%20%7B%22type%22%3A%20%22text%22%2C%22label%22%3A%20%222%22%7D%2C%22color%22%3A%20%22positive%22%7D%5D%2C%5B%7B%22action%22%3A%20%7B%22type%22%3A%20%22text%22%2C%22label%22%3A%20%223%22%7D%2C%22color%22%3A%20%22positive%22%7D%5D%2C%5B%7B%22action%22%3A%20%7B%22type%22%3A%20%22text%22%2C%22label%22%3A%20%224%22%7D%2C%22color%22%3A%20%22positive%22%7D%5D%2C%5B%7B%22action%22%3A%20%7B%22type%22%3A%20%22text%22%2C%22label%22%3A%20%225%22%7D%2C%22color%22%3A%20%22positive%22%7D%5D%5D%2C%22inline%22%3Afalse%7D"



run :: Monad m => Handle m -> StateT (T.Text,[(Int , Either OpenRepeat Int)]) m ()
run h = do
  jsonServ <- lift $ getLongPollServer h
  let ts = tsPollServ . response . fromJust . decode $ jsonServ
  modify $ foo ts
  forever $ runServ h jsonServ
  
runServ :: Monad m => Handle m -> LBS.ByteString -> StateT (T.Text,[(Int , Either OpenRepeat Int)]) m ()   
runServ h jsonServ = do
  let key =  extractKey $ jsonServ
  let server = extractServ $ jsonServ
  ts <- gets fst
  json <- lift $ getUpdates h key server ts
  let newTs = extractTs $ json
  modify $ foo newTs
  let upds = extractUpdates $ json
  mapM (chooseAction h) upds
  return ()

chooseAction :: Monad m => Handle m -> Update -> StateT (T.Text,[(Int , Either OpenRepeat Int)]) m ()
chooseAction h upd = do
  let usId = extractUserId $ upd  
  let msg = extractTextMsg $ upd
  db <- gets snd
  case lookup usId db of 
    Just (Left (OpenRepeat oldN)) -> do
      case checkButton msg of
        Just newN -> do
          modify $ func $ changeDB usId $ Right newN
          let infoMsg = T.pack $ "Number of repeats successfully changed from " ++ show oldN ++ " to " ++ show newN ++ "\n"
          lift $ sendMsg h usId infoMsg
          return ()
        Nothing -> do
          modify $ func $ changeDB usId $ Right oldN
          let infoMsg = T.pack $ "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++ show oldN ++ "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
          lift $ sendMsg h usId infoMsg
          return ()
    _ -> do
      let currN = case lookup usId db of { Just (Right n) -> n ; Nothing -> cStartN (hConf h) }
      case msg of  
        "/help" -> do
          let infoMsg = T.pack $ cHelpMsg (hConf h) 
          lift $ sendMsg h usId infoMsg
          return ()
        "/repeat" -> do
          let infoMsg = T.pack $ " : Current number of repeats your message.\n" ++ cRepeatQ (hConf h)
          lift $ sendKeyb h usId currN infoMsg
          modify $ func $ changeDB usId $ Left $ OpenRepeat currN 
        _ -> do 
          lift $ replicateM currN $ sendMsg h usId msg
          return ()



  



getLongPollServer' :: Handle IO -> IO LBS.ByteString
getLongPollServer' h = do
  req <- parseRequest $ "https://api.vk.com/method/groups.getLongPollServer?group_id=" ++ show (cGroupId (hConf h)) ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLBS req
  return (getResponseBody res)


getUpdates' :: T.Text -> T.Text -> T.Text -> IO LBS.ByteString
getUpdates' key server ts = do  
  req <- parseRequest $ T.unpack server ++ "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ T.unpack ts ++ "&wait=25"
  res <- httpLBS req
  return (getResponseBody res)

sendMsg' :: Handle IO -> Int -> T.Text -> IO LBS.ByteString
sendMsg' h usId msg = do
  req <- parseRequest $ "https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack msg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLBS req 
  return (getResponseBody res)

sendKeyb' :: Handle IO -> Int -> Int -> T.Text -> IO LBS.ByteString
sendKeyb' h usId n msg = do
  req <- parseRequest $ "https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ show n ++ T.unpack msg ++ "&keyboard=" ++ keyB ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLBS req
  return (getResponseBody res) 




extractUpdates :: LBS.ByteString -> [Update]
extractUpdates = updates . fromJust . decode 

extractTs :: LBS.ByteString -> T.Text
extractTs = ts . fromJust . decode

extractKey :: LBS.ByteString -> T.Text
extractKey = keyPollServ . response . fromJust . decode

extractServ :: LBS.ByteString -> T.Text
extractServ = serverPollServ . response . fromJust . decode

extractTextMsg :: Update -> T.Text
extractTextMsg = text . objectUpd 

extractUserId :: Update -> Int
extractUserId = from_id . objectUpd

foo :: a -> (a,b) -> (a,b)
foo x (y,z) = (x,z)

func :: (b -> b) -> (a,b) -> (a,b)
func f (a,b) = (a, f b)

changeDB :: Int -> Either OpenRepeat Int -> [(Int , Either OpenRepeat Int)] -> [(Int,Either OpenRepeat Int)]
changeDB usId eitherN bd = 
    case lookup usId bd of
        Just eitherX -> (:) (usId,eitherN) . delete (usId, eitherX) $ bd
        Nothing -> (:) (usId,eitherN) $ bd

checkButton :: T.Text -> Maybe Int
checkButton text =
    case text of 
      { "1" -> Just 1 ; "2" -> Just 2 ; "3" -> Just 3 ; "4" -> Just 4 ; "5" -> Just 5 ; _ -> Nothing }

  


