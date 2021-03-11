{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module App where

import           Logger
import           Api.Response
import           Api.Request
import           Network.HTTP.Client            ( withResponse, responseClose, brConsume, responseOpen, parseRequest, responseBody, httpLbs, method, requestBody, requestHeaders, RequestBody(..) )
import           Network.HTTP.Client.TLS        (newTlsManager)
import qualified Network.HTTP.Req               as R
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString                as BS
import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                      as T
import           Data.Maybe                     ( fromJust )
import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Control.Monad.Catch
import qualified Control.Exception              as E
import           Network.HTTP.Client.MultipartFormData
import           Data.Binary.Builder
import qualified System.IO                      as S


data Msg           = Msg        T.Text            deriving (Eq,Show)
data ToUserId      = ToUserId   Int               deriving (Eq,Show)

data VKBotException 
  = DuringGetLongPollServerException String
  | CheckGetServerResponseException String 
  | DuringGetUpdatesException String
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
    hLog              :: LogHandle m,
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

--enc :: ToJSON a => a -> LBS.ByteString
--enc = encode

--keyB = {"one_time": true,"buttons": [[{"action": {"type": "text","label": "1"},"color": "positive"}],[{"action": {"type": "text","label": "2"},"color": "positive"}],[{"action": {"type": "text","label": "3"},"color": "positive"}],[{"action": {"type": "text","label": "4"},"color": "positive"}],[{"action": {"type": "text","label": "5"},"color": "positive"}]],"inline":false}
--keyB1 = "{\"one_time\":true,\"buttons\":[[{\"action\": {\"type\":\"text\",\"label\":\"1\"},\"color\":\"positive\"}],[{\"action\": {\"type\":\"text\",\"label\":\"2\"},\"color\":\"positive\"}]],\"inline\":false}"

--keyB = "%7B%22one_time%22%3A%20true%2C%22buttons%22%3A%20%5B%5B%7B%22action%22%3A%20%7B%22type%22%3A%20%22text%22%2C%22label%22%3A%20%221%22%7D%2C%22color%22%3A%20%22positive%22%7D%5D%2C%5B%7B%22action%22%3A%20%7B%22type%22%3A%20%22text%22%2C%22label%22%3A%20%222%22%7D%2C%22color%22%3A%20%22positive%22%7D%5D%2C%5B%7B%22action%22%3A%20%7B%22type%22%3A%20%22text%22%2C%22label%22%3A%20%223%22%7D%2C%22color%22%3A%20%22positive%22%7D%5D%2C%5B%7B%22action%22%3A%20%7B%22type%22%3A%20%22text%22%2C%22label%22%3A%20%224%22%7D%2C%22color%22%3A%20%22positive%22%7D%5D%2C%5B%7B%22action%22%3A%20%7B%22type%22%3A%20%22text%22%2C%22label%22%3A%20%225%22%7D%2C%22color%22%3A%20%22positive%22%7D%5D%5D%2C%22inline%22%3Afalse%7D"

run :: (Monad m, MonadCatch m) => Handle m -> StateT (ServerInfo,[(Int , Either OpenRepeat Int)]) m ()
run h = do
  getServer h
  forever $ runServ h

getServer :: (Monad m, MonadCatch m) => Handle m -> StateT (ServerInfo,[(Int , Either OpenRepeat Int)]) m ()
getServer h = do
  lift $ logDebug (hLog h) $ "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=" ++ show (cGroupId (hConf h)) ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n"
  jsonServ <- lift $ getLongPollServer h `catch` (\e -> do
                                logError (hLog h) $ show e ++ " GetLongPollServer fail\n"
                                throwM $ DuringGetLongPollServerException $ show (e :: SomeException))
  lift $ logDebug (hLog h) ("Get response: " ++ show jsonServ ++ "\n")
  lift $ checkGetServResponse h jsonServ
  let ts = tsSI . response . fromJust . decode $ jsonServ
  let key =  extractKey $ jsonServ
  let server = extractServ $ jsonServ
  modify $ changeServerInfo (ServerInfo key server ts)

  
  
getUpdAndLog :: (Monad m, MonadCatch m) => Handle m -> StateT (ServerInfo,[(Int , Either OpenRepeat Int)]) m LBS.ByteString     
getUpdAndLog h = do
  ServerInfo key server ts <- gets fst
  lift $ logDebug (hLog h) $ "Send request to getUpdates: " ++ T.unpack server ++ "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ T.unpack ts ++ "&wait=25\n"
  json <- lift $ getUpdates h key server ts `catch` (\e -> do
                                logError (hLog h) $ show e ++ " GetUpdates fail\n"
                                throwM $ DuringGetUpdatesException $ show (e :: SomeException))
  lift $ logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  return json

runServ :: (Monad m, MonadCatch m) => Handle m -> StateT (ServerInfo,[(Int , Either OpenRepeat Int)]) m ()   
runServ h = do
  json <- getUpdAndLog h
  upds <- checkUpdates h json
  mapM_ (chooseAction h) upds

chooseAction :: (Monad m, MonadCatch m) => Handle m -> Update -> StateT (ServerInfo,[(Int , Either OpenRepeat Int)]) m ()
chooseAction h upd = do
  lift $ logInfo (hLog h) ("Analysis update from the list\n")
  case upd of
    UnknownUpdate _ -> do
      lift $ logWarning (hLog h) ("There is UNKNOWN UPDATE. BOT WILL IGNORE IT\n")
    Update {objectUpd = AboutObj {attachments = [PhotoAttachment {type' = _ }]}} -> do
      lift $ logWarning (hLog h) ("There is attachment update. BOT WILL IGNORE IT\n") 
    Update "message_new" (AboutObj usId id peerId txt [] []) -> do
      let msg = txt
      lift $ logInfo (hLog h) ("Get msg " ++ show msg ++ " from user " ++ show usId ++ "\n")
      db <- gets snd
      case lookup usId db of 
        Just (Left (OpenRepeat oldN)) -> do
          lift $ logInfo (hLog h) ("User " ++ show usId ++ " is in OpenRepeat mode\n")
          case checkButton msg of
            Just newN -> do
              lift $ logInfo (hLog h) ("Change number of repeats to " ++ show newN ++ " for user " ++ show usId ++ "\n")
              modify $ func $ changeDB usId $ Right newN
              let infoMsg = T.pack $ "Number of repeats successfully changed from " ++ show oldN ++ " to " ++ show newN ++ "\n"
              lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
              response <- lift $ sendMsg h usId infoMsg `catch` (\e -> do
                                    logError (hLog h) $ show e ++ " SendMessage fail\n"    
                                    throwM $ DuringSendMsgException (Msg infoMsg) (ToUserId usId) $ show (e :: SomeException))
              lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
              lift $ checkSendMsgResponse h usId infoMsg response
            Nothing -> do
              lift $ logWarning (hLog h) ("User " ++ show usId ++ " press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: " ++ show oldN ++ "\n")
              modify $ func $ changeDB usId $ Right oldN
              let infoMsg = T.pack $ "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++ show oldN ++ "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
              lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
              response <- lift $ sendMsg h usId infoMsg `catch` (\e -> do
                                    logError (hLog h) $ show e ++ " SendMessage fail\n"    
                                    throwM $ DuringSendMsgException (Msg infoMsg) (ToUserId usId) $ show (e :: SomeException))
              lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
              lift $ checkSendMsgResponse h usId infoMsg response
        _ -> do
          let currN = case lookup usId db of { Just (Right n) -> n ; Nothing -> cStartN (hConf h) }
          case filter ((/=) ' ') . T.unpack $ msg of  
            "/help" -> do
              let infoMsg = T.pack $ cHelpMsg (hConf h) 
              lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
              response <- lift $ sendMsg h usId infoMsg `catch` (\e -> do
                                    logError (hLog h) $ show e ++ " SendMessage fail\n"    
                                    throwM $ DuringSendMsgException (Msg infoMsg) (ToUserId usId) $ show (e :: SomeException))
              lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
              lift $ checkSendMsgResponse h usId infoMsg response
            "/repeat" -> do
              let infoMsg = T.pack $ " : Current number of repeats your message.\n" ++ cRepeatQ (hConf h)
              lift $ logDebug (hLog h) $ "Send request to send keyboard: https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ show currN ++ T.unpack infoMsg ++ "&keyboard=default_keyboard&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
              response <- lift $ sendKeyb h usId currN infoMsg `catch` (\e -> do
                                          logError (hLog h) $ show e ++ " SendKeyb fail\n" 
                                          throwM $ DuringSendKeybException (ToUserId usId) $ show (e :: SomeException))
              lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
              lift $ checkSendKeybResponse h usId currN infoMsg response
              modify $ func $ changeDB usId $ Left $ OpenRepeat currN 
            _ -> do 
              lift $ replicateM_ currN $ do
                logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack msg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
                response <- sendMsg h usId msg `catch` (\e -> do
                                    logError (hLog h) $ show e ++ " SendMessage fail\n"    
                                    throwM $ DuringSendMsgException (Msg msg) (ToUserId usId) $ show (e :: SomeException))
                logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
                checkSendMsgResponse h usId msg response
{-    Update "message_new" (AboutObj usId Id peerId txt [] attachs ) ->
      fmap answerAttachment h attachs

answerAttachment h usId (Attachment "photo" (Photo [])) = do
  logError (hLog h) $ show e ++ "Send attachment message fail. Unknown photo response, empty sizes\n"    
  throwM $ DuringSendMsgException (Msg "attachment") (ToUserId usId) $ "Send attachment message fail. Unknown photo response, empty sizes\n")
answerAttachment h usId (Attachment "photo" (Photo sizes)) = do
  let picUrl = url . head . reverse . sortOn height $ sizes
  serRespJson <- getPhotoServer h usId
  serUrl <- checkGetPhotoServResponse h serRespJson
  manager <- newTlsManager
  req1 <- parseRequest $ unpack picUrl
  res  <- httpLbs req1 manager
-}


loadPhotoToServ' serUrl picUrl = do
  manager <- newTlsManager
  req1 <- parseRequest $ T.unpack picUrl
  res  <- httpLbs req1 manager
  let bs = LBS.toStrict . responseBody $ res
  initReq2 <- parseRequest $ T.unpack serUrl
  req2     <- (formDataBody [partFileRequestBody "photo" (T.unpack picUrl) $ RequestBodyBS bs]
                initReq2)
  httpLbs req2 manager

{-loadPhotoToServ11 h serUrl picUrl = do
  manager <- newTlsManager
  req1 <- parseRequest $ T.unpack picUrl
  res  <- httpLbs req1 manager
  let bs = LBS.toStrict . responseBody $ res
  (fP,hl) <- S.openTempFile "./" "pic.jpg"
  S.hClose hl
  BS.writeFile fP bs
  S.hClose hl
  initReq2 <- parseRequest $ T.unpack serUrl
  req2     <- (formDataBody [partFileSource "photo" fP]
                initReq2)
  httpLbs req2 manager-}



checkGetServResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m ()
checkGetServResponse h json = do
  case decode json of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to getLongPollServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
      Just (ErrorAnswerServ { error'' =  _ } ) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to getLongPollServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "NEGATIVE RESPONSE:\n"   ++ show json
      Just _ -> do
        logInfo (hLog h) $ "Work with received server\n"

checkUpdates :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> StateT (ServerInfo,[(Int , Either OpenRepeat Int)]) m [Update]
checkUpdates h json = do
  case decode json of
      Nothing                      -> do
        lift $ logError (hLog h) $ "UNKNOWN RESPONSE to getUpdates:\n" ++ show json
        throwM $ CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
      Just (ErrorAnswer { error' = _ } ) -> do
        lift $ logError (hLog h) $ "NEGATIVE RESPONSE to getUpdates:\n" ++ show json
        throwM $ CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n"   ++ show json
      Just (FailAnswer 2 ) -> do
        lift $ logWarning (hLog h) ("FAIL. Long poll server key expired, need to request new key\n")
        getServer h
        json2 <- getUpdAndLog h
        checkUpdates h json2
      Just (FailAnswer 3 ) -> do
        lift $ logWarning (hLog h) ("FAIL. Long poll server information is lost, need to request new key and ts\n")
        getServer h
        json2 <- getUpdAndLog h
        checkUpdates h json2
      Just (FailTSAnswer {fail'' = 1 , ts'' = ts } ) -> do
        lift $ logWarning (hLog h) ("FAIL. Ts in request is wrong, need to use received ts\n")
        modify $ changeTs (T.pack . show $ ts)
        json2 <- getUpdAndLog h
        checkUpdates h json2
      Just (FailTSAnswer {fail'' = _ , ts'' = ts } ) -> do
        lift $ logWarning (hLog h) ("FAIL. Ts in request is wrong, need to use received ts\n")
        modify $ changeTs (T.pack . show $ ts)
        json2 <- getUpdAndLog h
        checkUpdates h json2
      Just (FailAnswer _ ) -> do
        lift $ logError (hLog h) $ "NEGATIVE RESPONSE to getUpdates:\n" ++ show json
        throwM $ CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n"   ++ show json
      Just (Answer { updates = [] }) -> do
        lift $ logInfo (hLog h) ("No new updates\n")
        return []
      Just (Answer ts upds) -> do
        modify $ changeTs ts
        lift $ logInfo (hLog h) ("There is new updates list\n")
        return upds

checkSendMsgResponse :: (Monad m, MonadCatch m) => Handle m -> Int -> T.Text -> LBS.ByteString -> m ()
checkSendMsgResponse h usId msg json = do
  case decode json of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to sendMessage:\n" ++ show json
        throwM $ CheckSendMsgResponseException (Msg msg) (ToUserId usId) $ "UNKNOWN RESPONSE:\n" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"  
      Just (ErrorAnswerMsg { error''' = _ } ) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to sendMessage:\n" ++ show json
        throwM $ CheckSendMsgResponseException (Msg msg) (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
      Just _                       -> do
        logInfo (hLog h) ("Msg " ++ show msg  ++ " was sent to user " ++ show usId ++ "\n")

checkSendKeybResponse :: (Monad m, MonadCatch m) => Handle m -> Int -> Int -> T.Text -> LBS.ByteString -> m ()
checkSendKeybResponse h usId n msg json = do
  case decode json of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to sendKeyboard:\n" ++ show json
        throwM $ CheckSendKeybResponseException (ToUserId usId) $ "UNKNOWN RESPONSE:\n" ++ show json ++ "\nKEYBOARD PROBABLY NOT SENT"  
      Just (ErrorAnswerMsg { error''' = _ } ) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to sendKeyboard:\n" ++ show json
        throwM $ CheckSendKeybResponseException (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nKEYBOARD NOT SENT"
      Just _                       -> do
        logInfo (hLog h) ("Keyboard with message: " ++ show n ++ show msg ++ " was sent to user " ++ show usId ++ "\n")



getLongPollServer' :: Handle IO -> IO LBS.ByteString
getLongPollServer' h = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/groups.getLongPollServer?group_id=" ++ show (cGroupId (hConf h)) ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

getPhotoServer' :: Handle IO -> Int -> IO LBS.ByteString
getPhotoServer' h usId = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/photos.getMessagesUploadServer?peer_id=" ++ show usId ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

checkGetPhotoServResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m T.Text
checkGetPhotoServResponse h json = do
  case decode json of
      Just (PhotoServerResponse serUrl) -> return serUrl
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to getPhotoServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

getUpdates' :: T.Text -> T.Text -> T.Text -> IO LBS.ByteString
getUpdates' key server ts = do
  manager <- newTlsManager  
  req <- parseRequest $ T.unpack server ++ "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ T.unpack ts ++ "&wait=25"
  res <- httpLbs req manager
  return (responseBody res)

sendMsg' :: Handle IO -> Int -> T.Text -> IO LBS.ByteString
sendMsg' h usId msg = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack msg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

sendKeyb' :: Handle IO -> Int -> Int -> T.Text -> IO LBS.ByteString
sendKeyb' h usId n msg = do
  let keyB = "{\"one_time\":true,\"buttons\":[[{\"action\":{\"type\":\"text\",\"label\":\"1\"},\"color\":\"positive\"}],[{\"action\":{\"type\":\"text\",\"label\":\"2\"},\"color\":\"positive\"}],[{\"action\":{\"type\":\"text\",\"label\":\"3\"},\"color\":\"positive\"}],[{\"action\":{\"type\":\"text\",\"label\":\"4\"},\"color\":\"positive\"}],[{\"action\":{\"type\":\"text\",\"label\":\"5\"},\"color\":\"positive\"}]],\"inline\":false}"
  let hT = R.https "api.vk.com" R./: "method" R./: "messages.send"
  let param1 = "user_id"      R.=: usId
  let param2 = "random_id"    R.=: (0 :: Int)
  let param3 = "message"      R.=: (show  n ++ T.unpack msg)
  let param4 = "keyboard"     R.=: (keyB :: T.Text)
  let param5 = "access_token" R.=: (T.pack . cBotToken $ (hConf h)) 
  let param6 = "v"            R.=: ("5.103" :: T.Text)
  let params = param1 <> param2 <> param3 <> param4 <> param5 <> param6 
  let body = R.ReqBodyUrlEnc params
  res <- R.req R.POST hT body R.lbsResponse mempty
  return (R.responseBody res) 

--req <- parseRequest $ "https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ show n ++ T.unpack msg ++ "&keyboard=" ++ keyB ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"

instance R.MonadHttp IO where
  handleHttpException e =  fail ""

--my :: IO R.LbsResponse
--my = R.req R.POST hT body R.lbsResponse mempty

extractUpdates :: LBS.ByteString -> [Update]
extractUpdates = updates . fromJust . decode 

extractTs :: LBS.ByteString -> T.Text
extractTs = ts . fromJust . decode

extractKey :: LBS.ByteString -> T.Text
extractKey = key . response . fromJust . decode

extractServ :: LBS.ByteString -> T.Text
extractServ = server . response . fromJust . decode

extractTextMsg :: Update -> T.Text
extractTextMsg = text . objectUpd 

extractUserId :: Update -> Int
extractUserId = from_id . objectUpd

changeServerInfo :: ServerInfo -> (ServerInfo,a) -> (ServerInfo,a)
changeServerInfo newInfo (oldInfo,a) = (newInfo,a)

changeTs :: T.Text -> (ServerInfo,a) -> (ServerInfo,a)
changeTs newTs ((ServerInfo key serv oldTs),a) = ((ServerInfo key serv newTs),a)

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

  


