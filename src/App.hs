{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module App where

import           Logger
import           Api.Response
import           Api.Request
import           Network.HTTP.Client            ( urlEncodedBody, parseRequest, responseBody, httpLbs, method, requestBody, requestHeaders, RequestBody(..) )
import           Network.HTTP.Client.TLS        (newTlsManager)
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
import           Data.String                    ( fromString )




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
    sendMsg           :: Int -> T.Text -> [Int] -> [String] -> String -> (String,String) -> m LBS.ByteString,
    sendKeyb          :: Int -> Int -> T.Text -> m LBS.ByteString,
    getPhotoServer    :: Int -> m LBS.ByteString,
    loadPhotoToServ   :: T.Text -> T.Text -> m LBS.ByteString,
    savePhotoOnServ   :: LoadPhotoResp -> m LBS.ByteString,
    getDocServer      :: Int -> String -> m LBS.ByteString,
    loadDocToServ     :: T.Text -> T.Text -> String -> m LBS.ByteString,
    saveDocOnServ     :: LoadDocResp -> String -> m LBS.ByteString
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
    Update "message_new" obj@(AboutObj usId id peerId txt fwds attachs) -> do
      let msg = txt
      lift $ logInfo (hLog h) ("Get msg " ++ show msg ++ " from user " ++ show usId ++ "\n")
      db <- gets snd
      case lookup usId db of 
        Just (Left (OpenRepeat oldN)) -> do
          lift $ logInfo (hLog h) ("User " ++ show usId ++ " is in OpenRepeat mode\n")
          case checkButton obj of
            Just newN -> do
              lift $ logInfo (hLog h) ("Change number of repeats to " ++ show newN ++ " for user " ++ show usId ++ "\n")
              modify $ func $ changeDB usId $ Right newN
              let infoMsg = T.pack $ "Number of repeats successfully changed from " ++ show oldN ++ " to " ++ show newN ++ "\n"
              lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
              response <- lift $ sendTxtMsg h usId infoMsg `catch` (\e -> do
                                    logError (hLog h) $ show e ++ " SendMessage fail\n"    
                                    throwM $ DuringSendMsgException (Msg infoMsg) (ToUserId usId) $ show (e :: SomeException))
              lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
              lift $ checkSendMsgResponse h usId infoMsg response
            Nothing -> do
              lift $ logWarning (hLog h) ("User " ++ show usId ++ " press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: " ++ show oldN ++ "\n")
              modify $ func $ changeDB usId $ Right oldN
              let infoMsg = T.pack $ "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++ show oldN ++ "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
              lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
              response <- lift $ sendTxtMsg h usId infoMsg `catch` (\e -> do
                                    logError (hLog h) $ show e ++ " SendMessage fail\n"    
                                    throwM $ DuringSendMsgException (Msg infoMsg) (ToUserId usId) $ show (e :: SomeException))
              lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
              lift $ checkSendMsgResponse h usId infoMsg response
        _ -> do
          let currN = case lookup usId db of { Just (Right n) -> n ; Nothing -> cStartN (hConf h) }
          case obj of
            AboutObj usId id peerId txt [] [] -> do
              chooseActionOfTxt h currN usId txt
            AboutObj usId id peerId txt [] [StickerAttachment "sticker" (StickerInfo idSt)] -> do
              lift $ replicateM_ currN $ sendStickMsg h usId idSt
            AboutObj usId id peerId txt [] attachs -> do
              eitherAttachStrings <- lift $ mapM (answerAttachment h usId) attachs
              case sequence eitherAttachStrings of
                Right attachStrings -> do
                  lift $ replicateM_ currN $ do 
                    response <- sendAttachMsg h usId txt attachStrings
                    checkSendMsgResponse h usId txt response
                Left str ->
                  lift $ logWarning (hLog h) ("There is UNKNOWN ATTACMENT in updateList. BOT WILL IGNORE IT. " ++ show attachs ++ "\n")     
            AboutObj usId id peerId txt fwds attachs -> do
              lift $ logWarning (hLog h) ("There is forward message. BOT WILL IGNORE IT. " ++ show upd ++ "\n")
    UnknownUpdate _ -> do
      lift $ logWarning (hLog h) ("There is UNKNOWN UPDATE. BOT WILL IGNORE IT. " ++ show upd ++ "\n")
    _ -> do
      lift $ logWarning (hLog h) ("There is UNKNOWN UPDATE. BOT WILL IGNORE IT. " ++ show upd ++ "\n")

chooseActionOfTxt :: (Monad m, MonadCatch m) => Handle m -> Int -> Int -> T.Text -> StateT (ServerInfo,[(Int , Either OpenRepeat Int)]) m ()
chooseActionOfTxt h currN usId msg = case filter ((/=) ' ') . T.unpack $ msg of
  "/help" -> do
    let infoMsg = T.pack $ cHelpMsg (hConf h) 
    lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
    response <- lift $ sendTxtMsg h usId infoMsg `catch` (\e -> do
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
      response <- sendTxtMsg h usId msg `catch` (\e -> do
                          logError (hLog h) $ show e ++ " SendMessage fail\n"    
                          throwM $ DuringSendMsgException (Msg msg) (ToUserId usId) $ show (e :: SomeException))
      logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
      checkSendMsgResponse h usId msg response

{-    Update {objectUpd = AboutObj {attachments = [PhotoAttachment {type' = _ }]}} -> do
      lift $ logWarning (hLog h) ("There is attachment update. BOT WILL IGNORE IT\n") 
UnknownUpdate _ -> do
      lift $ logWarning (hLog h) ("There is UNKNOWN UPDATE. BOT WILL IGNORE IT\n") -}

answerAttachment :: (Monad m, MonadCatch m) => Handle m -> Int -> Attachment -> m (Either String String)
answerAttachment h usId (PhotoAttachment "photo" (Photo [])) = do
  logError (hLog h) $ "Send attachment message fail. Unknown photo response, empty sizes\n"    
  throwM $ DuringSendMsgException (Msg "attachment") (ToUserId usId) $ "Send attachment message fail. Unknown photo response, empty sizes\n"
answerAttachment h usId (PhotoAttachment "photo" (Photo sizes)) = do
  let picUrl = url . head . reverse . sortOn height $ sizes
  serRespJson <- getPhotoServer h usId
  serUrl <- checkGetUploadServResponse h serRespJson
  loadPhotoJson <- loadPhotoToServ h serUrl picUrl
  loadPhotoResp <- checkLoadPhotoResponse h loadPhotoJson
  savePhotoJson <- savePhotoOnServ h loadPhotoResp
  (DocInfo id owner_id) <- checkSavePhotoResponse h savePhotoJson
  return $ Right $ "photo" ++ show owner_id ++ "_" ++ show id 
answerAttachment h usId (DocAttachment "doc" (Doc docUrl ext title)) = do
  serRespJson <- getDocServer h usId "doc"
  serUrl <- checkGetUploadServResponse h serRespJson
  loadDocJson <- loadDocToServ h serUrl docUrl ext
  loadDocResp <- checkLoadDocResponse h loadDocJson
  saveDocJson <- saveDocOnServ h loadDocResp title
  (DocInfo id owner_id) <- checkSaveDocResponse h saveDocJson
  return $ Right $ "doc" ++ show owner_id ++ "_" ++ show id 
answerAttachment h usId (AudioMesAttachment "audio_message" (Audio docUrl)) = do
  serRespJson <- getDocServer h usId "audio_message"
  serUrl <- checkGetUploadServResponse h serRespJson
  loadDocJson <- loadDocToServ h serUrl docUrl "ogg"
  loadDocResp <- checkLoadDocResponse h loadDocJson
  saveDocJson <- saveDocOnServ h loadDocResp "audio_message"
  (DocInfo id owner_id) <- checkSaveDocAuMesResponse h saveDocJson
  return $ Right $ "doc" ++ show owner_id ++ "_" ++ show id
answerAttachment h usId (VideoAttachment "video" (DocInfo id owner_id)) = do
  return $ Right $ "video" ++ show owner_id ++ "_" ++ show id 
answerAttachment h usId (StickerAttachment "sticker" (StickerInfo id)) = 
  return $ Right $ "doc" ++ show id ++ "_" ++ show id 
answerAttachment h usId (UnknownAttachment _) = return $ Left "unknown attachment"


loadDocToServ' :: T.Text -> T.Text -> String -> IO LBS.ByteString
loadDocToServ' serUrl docUrl ext = do
  manager <- newTlsManager
  req1 <- parseRequest $ T.unpack docUrl
  res1  <- httpLbs req1 manager
  let bs = LBS.toStrict . responseBody $ res1
  initReq2 <- parseRequest $ T.unpack serUrl
  req2     <- (formDataBody [partFileRequestBody "file" (T.unpack docUrl ++ " file." ++ ext) $ RequestBodyBS bs]
                initReq2)
  res2<- httpLbs req2 manager
  return (responseBody res2)

checkLoadDocResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m LoadDocResp
checkLoadDocResponse h json = do
  case decode json of
      Just loadDocResp@(LoadDocResp file) -> return loadDocResp
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to loadDocToServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

saveDocOnServ' :: Handle IO -> LoadDocResp -> String -> IO LBS.ByteString
saveDocOnServ' h (LoadDocResp file) title = do
  manager <- newTlsManager
  initReq <- parseRequest $ "https://api.vk.com/method/docs.save"
  let param1 = ("file"        , fromString file)
  let param2 = ("title"       , fromString title)
  let param3 = ("access_token", fromString (cBotToken $ (hConf h))) 
  let param4 = ("v"           ,"5.103")
  let params = param1 : param2 : param3 : param4 : []   
  let req = urlEncodedBody params initReq
  res <- httpLbs req manager
  return (responseBody res)

checkSaveDocResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m DocInfo
checkSaveDocResponse h json = do
  case decode json of
      Just (SaveDocResp (ResponseSDR "doc" (DocInfo id ownerId) )) -> return (DocInfo id ownerId)
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to saveDocOnServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

checkSaveDocAuMesResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m DocInfo
checkSaveDocAuMesResponse h json = do
  case decode json of
      Just (SaveDocAuMesResp (ResponseSDAMR "audio_message" (DocInfo id ownerId) )) -> return (DocInfo id ownerId)
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to saveDocAudioMesOnServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

loadPhotoToServ' :: T.Text -> T.Text -> IO LBS.ByteString
loadPhotoToServ' serUrl picUrl = do
  manager <- newTlsManager
  req1 <- parseRequest $ T.unpack picUrl
  res1  <- httpLbs req1 manager
  let bs = LBS.toStrict . responseBody $ res1
  initReq2 <- parseRequest $ T.unpack serUrl
  req2     <- (formDataBody [partFileRequestBody "photo" (T.unpack picUrl) $ RequestBodyBS bs]
                initReq2)
  res2<- httpLbs req2 manager
  return (responseBody res2)

checkLoadPhotoResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m LoadPhotoResp
checkLoadPhotoResponse h json = do
  case decode json of
      Just loadPhotoResp@(LoadPhotoResp server hash photo) -> return loadPhotoResp
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to loadPhotoToServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

savePhotoOnServ' :: Handle IO -> LoadPhotoResp -> IO LBS.ByteString
savePhotoOnServ' h (LoadPhotoResp server hash photo) = do
  manager <- newTlsManager
  initReq <- parseRequest $ "https://api.vk.com/method/photos.saveMessagesPhoto"
  let param1 = ("server"      , fromString . show $ server) 
  let param2 = ("hash"        , fromString hash)
  let param3 = ("photo"       , fromString photo)
  let param4 = ("access_token", fromString (cBotToken $ (hConf h))) 
  let param5 = ("v"           ,"5.103")
  let params = param1 : param2 : param3 : param4 : param5 : [] 
  let req = urlEncodedBody params initReq
  res <- httpLbs req manager
  return (responseBody res)

checkSavePhotoResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m DocInfo
checkSavePhotoResponse h json = do
  case decode json of
      Just (SavePhotoResp [DocInfo id ownerId ]) -> return (DocInfo id ownerId )
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to savePhotoOnServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

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

getDocServer' :: Handle IO -> Int -> String -> IO LBS.ByteString
getDocServer' h usId type' = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/docs.getMessagesUploadServer?type=" ++ type' ++ "&peer_id=" ++ show usId ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)



getPhotoServer' :: Handle IO -> Int -> IO LBS.ByteString
getPhotoServer' h usId = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/photos.getMessagesUploadServer?peer_id=" ++ show usId ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

checkGetUploadServResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m T.Text
checkGetUploadServResponse h json = do
  case decode json of
      Just (UploadServerResponse (UploadUrl serUrl)) -> return serUrl
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to getPhotoServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

getUpdates' :: T.Text -> T.Text -> T.Text -> IO LBS.ByteString
getUpdates' key server ts = do
  manager <- newTlsManager  
  req <- parseRequest $ T.unpack server ++ "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ T.unpack ts ++ "&wait=25"
  res <- httpLbs req manager
  return (responseBody res)

sendMsg' :: Handle IO -> Int -> T.Text -> [Int] -> [String] -> String -> (String,String) -> IO LBS.ByteString
sendMsg' h usId txt fwds attachStrings stickerId (lat,long)  = do
  manager <- newTlsManager
  let param1  = "user_id=" ++ show usId
  let param2  = "random_id=0"
  let param3  = "message=" ++ T.unpack txt
  let param4  = "forward_messages=" ++ (intercalate "," . fmap show $ fwds) 
  let param5  = "attachment=" ++ intercalate "," attachStrings
  let param6  = "sticker_id=" ++ stickerId
  let param7  = "lat=" ++ lat
  let param8  = "long=" ++ long
  let param9  = "access_token=" ++ cBotToken (hConf h)
  let param10 = "v=5.103"
  let params  = intercalate "&" (param1:param2:param3:param4:param5:param6:param7:param8:param9:param10:[])
  req <- parseRequest $ "https://api.vk.com/method/messages.send?" ++ params
  res <- httpLbs req manager
  return (responseBody res)

sendTxtMsg h usId txt = sendMsg h usId txt [] [] "" ("","")
sendAttachMsg h usId txt attachStrs = sendMsg h usId txt [] attachStrs "" ("","")

sendStickMsg :: (Monad m, MonadCatch m) => Handle m -> Int -> Int -> m LBS.ByteString
sendStickMsg  h usId stickerId  = sendMsg h usId "" [] [] (show stickerId) ("","")



sendKeyb' :: Handle IO -> Int -> Int -> T.Text -> IO LBS.ByteString
sendKeyb' h usId n msg = do
  manager <- newTlsManager
  initReq <- parseRequest $ "https://api.vk.com/method/messages.send"
  let param1 = ("user_id"     , fromString . show $ usId) 
  let param2 = ("random_id"   ,"0")
  let param3 = ("message"     , fromString (show  n ++ T.unpack msg))
  let param4 = ("keyboard"    , LBS.toStrict . encode $ kB)
  let param5 = ("access_token", fromString (cBotToken $ (hConf h))) 
  let param6 = ("v"           ,"5.103")
  let params = param1 : param2 : param3 : param4 : param5 : param6 : [] 
  let req = urlEncodedBody params initReq
  res <- httpLbs req manager
  return (responseBody res)


extractUpdates :: LBS.ByteString -> [Update]
extractUpdates = updates . fromJust . decode 

extractTs :: LBS.ByteString -> T.Text
extractTs = tsSI . fromJust . decode

extractKey :: LBS.ByteString -> T.Text
extractKey = key . response . fromJust . decode

extractServ :: LBS.ByteString -> T.Text
extractServ = serverSI . response . fromJust . decode

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


checkButton :: AboutObj -> Maybe Int
checkButton obj =
  case obj of
    AboutObj usId id peerId txt [] [] -> checkTextButton txt
    _  -> Nothing

checkTextButton :: T.Text -> Maybe Int
checkTextButton text =
    case text of 
      { "1" -> Just 1 ; "2" -> Just 2 ; "3" -> Just 3 ; "4" -> Just 4 ; "5" -> Just 5 ; _ -> Nothing }

  


