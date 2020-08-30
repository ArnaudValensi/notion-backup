{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module GetTasks (getTasksRequest, getUrlFromTaskResponse) where

import Control.Concurrent.MonadIO (MonadIO)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text as T
import Network.HTTP.Simple
import Utils
  ( buildNotionRequest,
    processResponse,
  )

-- Response shape:
--
-- {
--   "results": [
--     {
--       "id": "00000000-0000-0000-0000-000000000000",
--       "eventName": "exportBlock",
--       "request": {
--         "blockId": "00000000-0000-0000-0000-000000000000",
--         "recursive": false,
--         "exportOptions": {
--           "exportType": "markdown",
--           "timeZone": "Europe/Paris",
--           "locale": "en"
--         }
--       },
--       "actor": {
--         "table": "notion_user",
--         "id": "00000000-0000-0000-0000-000000000000"
--       },
--       "state": "success",
--       "status": {
--         "type": "complete",
--         "pagesExported": 1,
--         "exportURL": "https://link.zip%22"
--       }
--     }
--   ]
-- }
data GetTaskResponse = GetTaskResponse
  { resultsGetTaskResponse :: [ResultElement]
  }
  deriving (Show)

data ResultElement = ResultElement
  { resultIDResultElement :: Text,
    eventNameResultElement :: Text,
    requestResultElement :: TaskRequest,
    actorResultElement :: Actor,
    stateResultElement :: Text,
    statusResultElement :: Status
  }
  deriving (Show)

data Actor = Actor
  { tableActor :: Text,
    actorIDActor :: Text
  }
  deriving (Show)

data TaskRequest = TaskRequest
  { blockIDRequest :: Text,
    recursiveRequest :: Bool,
    exportOptionsRequest :: ExportOptions
  }
  deriving (Show)

data ExportOptions = ExportOptions
  { exportTypeExportOptions :: Text,
    timeZoneExportOptions :: Text,
    localeExportOptions :: Text
  }
  deriving (Show)

data Status = Status
  { statusTypeStatus :: Text,
    pagesExportedStatus :: Int,
    exportURLStatus :: Text
  }
  deriving (Show)

decodeGetTaskResponse :: LC.ByteString -> Either String GetTaskResponse
decodeGetTaskResponse = eitherDecode

instance ToJSON GetTaskResponse where
  toJSON (GetTaskResponse resultsGetTaskResponse) =
    object
      [ "results" .= resultsGetTaskResponse
      ]

instance FromJSON GetTaskResponse where
  parseJSON (Object v) =
    GetTaskResponse
      <$> v .: "results"

instance ToJSON ResultElement where
  toJSON (ResultElement resultIDResultElement eventNameResultElement requestResultElement actorResultElement stateResultElement statusResultElement) =
    object
      [ "id" .= resultIDResultElement,
        "eventName" .= eventNameResultElement,
        "request" .= requestResultElement,
        "actor" .= actorResultElement,
        "state" .= stateResultElement,
        "status" .= statusResultElement
      ]

instance FromJSON ResultElement where
  parseJSON (Object v) =
    ResultElement
      <$> v .: "id"
      <*> v .: "eventName"
      <*> v .: "request"
      <*> v .: "actor"
      <*> v .: "state"
      <*> v .: "status"

instance ToJSON Actor where
  toJSON (Actor tableActor actorIDActor) =
    object
      [ "table" .= tableActor,
        "id" .= actorIDActor
      ]

instance FromJSON Actor where
  parseJSON (Object v) =
    Actor
      <$> v .: "table"
      <*> v .: "id"

instance ToJSON TaskRequest where
  toJSON (TaskRequest blockIDRequest recursiveRequest exportOptionsRequest) =
    object
      [ "blockId" .= blockIDRequest,
        "recursive" .= recursiveRequest,
        "exportOptions" .= exportOptionsRequest
      ]

instance FromJSON TaskRequest where
  parseJSON (Object v) =
    TaskRequest
      <$> v .: "blockId"
      <*> v .: "recursive"
      <*> v .: "exportOptions"

instance ToJSON ExportOptions where
  toJSON (ExportOptions exportTypeExportOptions timeZoneExportOptions localeExportOptions) =
    object
      [ "exportType" .= exportTypeExportOptions,
        "timeZone" .= timeZoneExportOptions,
        "locale" .= localeExportOptions
      ]

instance FromJSON ExportOptions where
  parseJSON (Object v) =
    ExportOptions
      <$> v .: "exportType"
      <*> v .: "timeZone"
      <*> v .: "locale"

instance ToJSON Status where
  toJSON (Status statusTypeStatus pagesExportedStatus exportURLStatus) =
    object
      [ "type" .= statusTypeStatus,
        "pagesExported" .= pagesExportedStatus,
        "exportURL" .= exportURLStatus
      ]

instance FromJSON Status where
  parseJSON (Object v) =
    Status
      <$> v .: "type"
      <*> v .: "pagesExported"
      <*> v .: "exportURL"

getTaskEndpoint :: BC.ByteString
getTaskEndpoint = "/api/v3/getTasks"

buildGetTasksBody :: String -> LC.ByteString
buildGetTasksBody taskId = "{\"taskIds\":[\"" <> LC.pack taskId <> "\"]}"

getTasksRequest :: String -> String -> Request
getTasksRequest token taskId = buildNotionRequest (BC.pack token) getTaskEndpoint $ buildGetTasksBody taskId

getExportUrl :: GetTaskResponse -> Text
getExportUrl getTaskResponse = exportURLStatus . statusResultElement . Prelude.head . resultsGetTaskResponse $ getTaskResponse

getUrlFromTaskResponse :: Response L.ByteString -> Either String Text
getUrlFromTaskResponse response =
  processResponse response
    >>= (\processedResponse -> decodeGetTaskResponse processedResponse)
    >>= \getTaskResponse -> return $ getExportUrl getTaskResponse
