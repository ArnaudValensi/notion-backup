{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module ExportPageCommand (exportPageCommand) where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text as T
import EnqueueTask
  ( decodeEnqueueTaskResponse,
    requestEnqueueTask,
    taskIDEnqueueTaskResponse,
  )
import GetTasks
  ( getTasksRequest,
    getUrlFromTaskResponse,
  )
import Network.HTTP.Simple
import Utils
  ( buildNotionRequest,
    processResponse,
  )

processEnqueueTaskResponse :: Response L.ByteString -> Either String String
processEnqueueTaskResponse enqueueTaskResponse =
  processResponse enqueueTaskResponse
    >>= (\processedResponse -> decodeEnqueueTaskResponse processedResponse)
    >>= (\enqueueTaskResponse -> return $ T.unpack $ taskIDEnqueueTaskResponse enqueueTaskResponse)

download :: Text -> IO (Either String L.ByteString)
download url = do
  request <- parseRequest $ T.unpack url
  response <- httpLBS request
  return $ processResponse response

-- Enqueue task.
-- Deserializa json response.
-- Get task id.
-- Get tasks.
-- Deserialize json response.
-- Download from link.

-- TODO: Flatten this using monad transformers? See https://stackoverflow.com/questions/52016330/haskell-common-pattern-to-deal-with-failure-inside-io-io-either-string-int
-- TODO: Improve error messages.
exportPageCommand :: String -> String -> IO ()
exportPageCommand token pageId = do
  enqueueTaskResponse <- requestEnqueueTask token pageId
  case processEnqueueTaskResponse enqueueTaskResponse of
    Left errorMessage -> print $ "Error: " <> errorMessage
    Right taskId -> do
      print $ "taskId: " <> taskId
      getTasksResponse <- httpLBS $ getTasksRequest token taskId
      case getUrlFromTaskResponse getTasksResponse of
        Left errorMessage -> print $ "Error2: " <> errorMessage
        Right fileUrl -> do
          print $ "Downloading archive: " <> fileUrl
          eitherFileBytes <- download fileUrl
          case eitherFileBytes of
            Left errorMessage -> print $ "Error3: " <> errorMessage
            Right fileBytes -> L.writeFile "page.zip" fileBytes
