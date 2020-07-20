{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           App
import           Logger
import qualified Data.Text                      as T
import qualified Data.ByteString.Lazy           as LBS
import           Control.Monad.State


data MockAction = GOTSERVER | GOTUPDATES T.Text T.Text T.Text | SENDMSG Int T.Text | SENDKEYB Int Int T.Text | LOGMSG Priority String
                                       deriving (Eq,Show)

getServerTest:: LBS.ByteString -> StateT [MockAction] IO LBS.ByteString
getServerTest json = StateT $ \s -> return ( json , GOTSERVER : s)

getUpdatesTest:: LBS.ByteString -> T.Text -> T.Text -> T.Text -> StateT [MockAction] IO LBS.ByteString
getUpdatesTest json key server ts = StateT $ \s -> return ( json , GOTUPDATES key server ts : s)

sendMsgTest :: LBS.ByteString -> Int -> T.Text -> StateT [MockAction] IO LBS.ByteString
sendMsgTest json usId msg = StateT $ \s -> 
    return ( json , (SENDMSG usId msg) : s)

sendKeybTest :: LBS.ByteString -> Int -> Int -> T.Text-> StateT [MockAction] IO LBS.ByteString
sendKeybTest json usId currN msg = StateT $ \s -> 
    return ( json , (SENDKEYB usId currN msg) : s)

logTest :: Priority -> String -> StateT [MockAction] IO ()
logTest prio text = StateT $ \s -> 
    return (() , LOGMSG prio text : s)

config1 = Config { cStartN = 2 , cBotToken = "ABC123" , cHelpMsg = "Hello" , cRepeatQ = "Why?", cGroupId = 321}
handleLog1 = LogHandle (LogConfig DEBUG) logTest
handle1 = Handle { hConf = config1,
                   hLog = handleLog1,
                   getLongPollServer = getServerTest json1,
                   getUpdates = getUpdatesTest json2,
                   sendMsg = sendMsgTest json4,
                   sendKeyb = sendKeybTest json4}

handle2 = handle1 {getUpdates = getUpdatesTest json3}
handle3 = handle1 {getLongPollServer = getServerTest json5}
handle4 = handle1 {getLongPollServer = getServerTest json6}
handle5 = handle1 {getUpdates = getUpdatesTest json7}
handle6 = handle1 {getUpdates = getUpdatesTest json5}
handle7 = handle1 {getUpdates = getUpdatesTest json6}

initialDB1 = []

main :: IO ()
main = hspec $ do
  describe "getServer" $ do
    it "throw Exception with error answer" $ do
      evalStateT (evalStateT (getServer handle3) ("1",initialDB1) ) [] 
        `shouldThrow` ( == (CheckGetServerResponseException $ "NEGATIVE RESPONSE:\n" ++ show json5))

    it "throw CheckGetServerResponseException with unknown answer" $ do
      evalStateT (evalStateT (getServer handle4) ("1",initialDB1) ) [] 
        `shouldThrow` ( == (CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n" ++ show json6))    
  
  describe "(getServer >>= runServ)" $ do
    it "work with empty update list" $ do
      state <- execStateT (evalStateT (getServer handle1 >>= runServ handle1) ("1",initialDB1) ) []
      reverse state `shouldBe`
        [LOGMSG DEBUG "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=321&access_token=ABC123&v=5.103\n", 
        GOTSERVER,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO $ "Work with received server\n",
        LOGMSG DEBUG $ "Send request to getUpdates: https://lp.vk.com/wh000?act=a_check&key=912481cc91cb3b0e119b9be5c75b383d6887438f&ts=289&wait=25\n",
        GOTUPDATES "912481cc91cb3b0e119b9be5c75b383d6887438f" "https://lp.vk.com/wh000" "289",
        LOGMSG DEBUG $ "Get response: " ++ show json2 ++ "\n",
        LOGMSG INFO "No new updates\n"]

    it "work with singleton update list with text msg" $ do
      state <- execStateT (evalStateT (getServer handle2 >>= runServ handle2) ("1",initialDB1) ) []
      reverse state `shouldBe`
        [LOGMSG DEBUG "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=321&access_token=ABC123&v=5.103\n", 
        GOTSERVER,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO $ "Work with received server\n",
        LOGMSG DEBUG $ "Send request to getUpdates: https://lp.vk.com/wh000?act=a_check&key=912481cc91cb3b0e119b9be5c75b383d6887438f&ts=289&wait=25\n",
        GOTUPDATES "912481cc91cb3b0e119b9be5c75b383d6887438f" "https://lp.vk.com/wh000" "289",
        LOGMSG DEBUG $ "Get response: " ++ show json3 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG INFO "Analysis update from the list\n",
        LOGMSG INFO $ "Get msg \"love\" from user 123\n"] ++
        (concat . replicate 2 $ 
          [LOGMSG DEBUG "Send request to send msg https://api.vk.com/method/messages.send?user_id=123&random_id=0&message=love&access_token=ABC123&v=5.103\n" ,
          SENDMSG 123 "love",
          LOGMSG DEBUG "Get response: \"{\\\"response\\\":626}\"\n",
          LOGMSG INFO "Msg \"love\" was sent to user 123\n"])
    
    it "work with singleton update list with gif msg (do nothing)" $ do
      state <- execStateT (evalStateT (getServer handle5 >>= runServ handle5) ("1",initialDB1) ) []
      reverse state `shouldBe`
        [LOGMSG DEBUG "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=321&access_token=ABC123&v=5.103\n", 
        GOTSERVER,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO $ "Work with received server\n",
        LOGMSG DEBUG $ "Send request to getUpdates: https://lp.vk.com/wh000?act=a_check&key=912481cc91cb3b0e119b9be5c75b383d6887438f&ts=289&wait=25\n",
        GOTUPDATES "912481cc91cb3b0e119b9be5c75b383d6887438f" "https://lp.vk.com/wh000" "289",
        LOGMSG DEBUG $ "Get response: " ++ show json7 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG INFO "Analysis update from the list\n",
        LOGMSG WARNING "There is attachment update. BOT WILL IGNORE IT\n"] 
    
    it "throw CheckGetUpdatesException with error answer" $ do
      evalStateT (evalStateT (getServer handle6 >>= runServ handle6) ("1",initialDB1) ) []
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show json5))
    
    it "throw CheckGetUpdatesException with unknown answer" $ do
      evalStateT (evalStateT (getServer handle7 >>= runServ handle7) ("1",initialDB1) ) []
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n" ++ show json6))

json1 = "{\"response\":{\"key\":\"912481cc91cb3b0e119b9be5c75b383d6887438f\",\"server\":\"https:\\/\\/lp.vk.com\\/wh000\",\"ts\":\"289\"}}"
json2 = "{\"ts\":\"289\",\"updates\":[]}\r\n"
json3 = "{\"ts\":\"290\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1594911394,\"from_id\":123,\"id\":597,\"out\":0,\"peer_id\":16063921,\"text\":\"love\",\"conversation_message_id\":562,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"group_id\":194952914,\"event_id\":\"35ec397e45dfe993d365912ea32be41be5e77a0c\"}]}\r\n"
json4 = "{\"response\":626}"
json5 = "{\"error\":{\"error_code\":5,\"error_msg\":\"User authorization failed: invalid access_token (4).\",\"request_params\":[{\"key\":\"user_id\",\"value\":\"16063921\"},{\"key\":\"random_id\",\"value\":\"0\"},{\"key\":\"v\",\"value\":\"5.103\"},{\"key\":\"method\",\"value\":\"messages.send\"},{\"key\":\"oauth\",\"value\":\"1\"}]}}"
json6 = "lalala"
json7 = "{\"ts\":\"304\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1594932378,\"from_id\":16063921,\"id\":629,\"out\":0,\"peer_id\":16063921,\"text\":\"\",\"conversation_message_id\":594,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[{\"type\":\"sticker\",\"sticker\":{\"product_id\":279,\"sticker_id\":9014,\"images\":[{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-64\",\"width\":64,\"height\":64},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-128\",\"width\":128,\"height\":128},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-256\",\"width\":256,\"height\":256},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-352\",\"width\":352,\"height\":352},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-512\",\"width\":512,\"height\":512}],\"images_with_background\":[{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-64b\",\"width\":64,\"height\":64},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-128b\",\"width\":128,\"height\":128},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-256b\",\"width\":256,\"height\":256},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-352b\",\"width\":352,\"height\":352},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-512b\",\"width\":512,\"height\":512}]}}],\"is_hidden\":false},\"group_id\":194952914,\"event_id\":\"a3d68972637b90446ac3be5a171d923fa0f10f31\"}]}\r\n"


