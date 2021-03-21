{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           Logger
import           Api.Response
import qualified Data.Configurator              as C
import qualified Data.Configurator.Types        as C
import qualified Data.Text                      as T
import qualified Data.ByteString.Lazy           as LBS
import           Control.Monad.State
import           Data.Char
import           Control.Exception
import           System.IO
import           Data.Time.Clock
import           Data.Time.LocalTime


pullConfig :: IO C.Config
pullConfig = do
  C.load [C.Required "./bot.config"] 
    `catch` (\e -> putStrLn (show (e :: C.ConfigError)) >> return C.empty)
    `catch` (\e -> putStrLn (show (e :: C.KeyError   )) >> return C.empty)
    `catch` (\e -> putStrLn (show (e :: IOException  )) >> return C.empty)
    `catch` (\e -> throw $ DuringPullConfigException  $ show (e :: SomeException))


main :: IO ()
main = do
  time <- getTime                          
  let currLogPath = "./VK.LogSession: " ++ show time ++ " bot.log"
  writeFile currLogPath  "Create log file\n"
  conf           <- pullConfig             
  startN         <- parseConfStartN   conf 
  botToken       <- parseConfBotToken conf
  prio           <- parseConfPrio     conf  
  helpMsg        <- parseConfHelpMsg  conf 
  repeatQuestion <- parseConfRepeatQ  conf
  groupId        <- parseConfGroupId  conf 
  let config = Config startN botToken helpMsg repeatQuestion groupId
  let handleLog = LogHandle (LogConfig prio) (logger handleLog currLogPath)
  let handle = Handle config handleLog (getLongPollServer' handle) getUpdates' (sendMsg' handle) (sendKeyb' handle) (getPhotoServer' handle) loadPhotoToServ' (savePhotoOnServ' handle) (getDocServer' handle) loadDocToServ' (saveDocOnServ' handle) goToUrl'
  putStrLn "App started"
  evalStateT (forever $ run handle) (ServerInfo "A" "A" "1" ,[])
  


getTime :: IO String
getTime = (do
  time    <- getZonedTime
  return $ show time)     
    `catch` (\e -> ((putStrLn $ show (e :: SomeException)) >> inputLocalTime) )


parseConfStartN :: C.Config -> IO Int
parseConfStartN conf = (do
  str <- ((C.lookup conf "VK.startN") :: IO (Maybe Int))
    `catch` ( (\e -> return Nothing) :: C.KeyError  -> IO (Maybe Int) )
    `catch` ( (\e -> return Nothing) :: IOException -> IO (Maybe Int) ) 
  case str of
    Nothing -> inputStartN
    Just 1  -> return 1
    Just 2  -> return 2
    Just 3  -> return 3
    Just 4  -> return 4
    Just 5  -> return 5
    Just _  -> inputStartN)
      `catch` (\e -> throw $ DuringParseConfigException $ "startN\n" ++ show (e :: SomeException))

parseConfBotToken :: C.Config -> IO String
parseConfBotToken conf = (do
  str <- ((C.lookup conf "VK.botToken") :: IO (Maybe String))
    `catch` ( (\e -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `catch` ( (\e -> return Nothing) :: IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputBotToken
    Just n  -> return n)
      `catch` (\e -> throw $ DuringParseConfigException $ "botToken\n" ++ show (e :: SomeException))
         
parseConfPrio :: C.Config -> IO Priority
parseConfPrio conf = (do
  str <- (C.lookup conf "VK.logLevel" :: IO (Maybe String))
    `catch` ( (\e -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `catch` ( (\e -> return Nothing) :: IOException -> IO (Maybe String) ) 
  case str of
    Nothing        -> inputLogLevel
    Just "DEBUG"   -> return DEBUG
    Just "INFO"    -> return INFO
    Just "WARNING" -> return WARNING
    Just "ERROR"   -> return ERROR
    Just _         -> inputLogLevel)
      `catch` (\e -> throw $ DuringParseConfigException $ "logLevel\n" ++ show (e :: SomeException))

parseConfHelpMsg :: C.Config -> IO String
parseConfHelpMsg conf = (do
  str <- ((C.lookup conf "VK.help_Info_Msg") :: IO (Maybe String))
    `catch` ( (\e -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `catch` ( (\e -> return Nothing) :: IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputHelpMsg
    Just n  -> return n)
      `catch` (\e -> throw $ DuringParseConfigException $ "helpMsg\n" ++ show (e :: SomeException))

parseConfRepeatQ :: C.Config -> IO String
parseConfRepeatQ conf = (do
  str <- ((C.lookup conf "VK.repeat_Info_Question") :: IO (Maybe String))
    `catch` ( (\e -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `catch` ( (\e -> return Nothing) :: IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputRepeatQ
    Just n  -> return n)
      `catch` (\e -> throw $ DuringParseConfigException $ "repeatQuestion\n" ++ show (e :: SomeException))

parseConfGroupId :: C.Config -> IO Int
parseConfGroupId conf = (do
  str <- ((C.lookup conf "VK.group_id") :: IO (Maybe Int))
    `catch` ( (\e -> return Nothing) :: C.KeyError  -> IO (Maybe Int) )
    `catch` ( (\e -> return Nothing) :: IOException -> IO (Maybe Int) ) 
  case str of
    Nothing -> inputGroupId
    Just n  -> return n)
      `catch` (\e -> throw $ DuringParseConfigException $ "repeatQuestion\n" ++ show (e :: SomeException))



inputStartN :: IO Int
inputStartN = do
  putStrLn "Can`t parse value \"startN\" from configuration file or command line\nPlease, enter start number of repeats. Number from 1 to 5"
  input <- getLine
  case input of
    "1" -> return 1
    "2" -> return 2
    "3" -> return 3
    "4" -> return 4
    "5" -> return 5
    _   -> inputStartN

inputBotToken :: IO String
inputBotToken = do
  putStrLn "Can`t parse value \"botToken\" from configuration file or command line\nPlease, enter bot token"
  getLine

inputLogLevel :: IO Priority
inputLogLevel = do
  putStrLn "Can`t parse value \"logLevel\" from configuration file or command line\nPlease, enter logging level (logs of this level and higher will be recorded)\nAvailable levels: DEBUG ; INFO ; WARNING ; ERROR (without quotes)"
  input <- getLine
  case (map toUpper input) of
    "DEBUG"   -> return DEBUG
    "INFO"    -> return INFO
    "WARNING" -> return WARNING
    "ERROR"   -> return ERROR
    _         -> inputLogLevel

inputHelpMsg :: IO String
inputHelpMsg = do
  putStrLn "Can`t parse value \"/help Info Msg\" from configuration file or command line\nPlease, enter \"/help Info Msg\"\nExample: I`m super bot"
  getLine

inputRepeatQ :: IO String
inputRepeatQ = do
  putStrLn "Can`t parse value \"/repeat Info Question\" from configuration file or command line\nPlease, enter \"/repeat Info Question\"\nExample: How many times to repeat message in the future?"
  getLine

inputGroupId :: IO Int
inputGroupId = do
  putStrLn "Can`t parse value \"group id\" from configuration file or command line\nPlease, enter NUMBER of group id\nExample: 123456789"
  str <- getLine
  case all isNumber str of 
      True -> do
        let grId = read str 
        return grId
      _ -> inputGroupId

inputLocalTime :: IO String
inputLocalTime = (do
  putStrLn "Local time not found\nPlease, enter your local time in any form\nExample: 06.07.2020 16:21"
  getLine) 
    `catch` (\e -> throw $ DuringGetTimeException $ show (e :: SomeException))
