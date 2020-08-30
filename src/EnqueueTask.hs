{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module EnqueueTask
  ( requestEnqueueTask,
    taskIDEnqueueTaskResponse,
    decodeEnqueueTaskResponse,
  )
where

import Control.Concurrent.MonadIO (MonadIO)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text as T
import Network.HTTP.Simple
import Utils (buildNotionRequest)

enqueueTaskEndpoint :: BC.ByteString
enqueueTaskEndpoint = "/api/v3/enqueueTask"

data EnqueueTaskResponse = EnqueueTaskResponse
  { taskIDEnqueueTaskResponse :: Text
  }
  deriving (Show)

decodeEnqueueTaskResponse :: LC.ByteString -> Either String EnqueueTaskResponse
decodeEnqueueTaskResponse = eitherDecode

instance ToJSON EnqueueTaskResponse where
  toJSON (EnqueueTaskResponse taskIDEnqueueTaskResponse) =
    object
      [ "taskId" .= taskIDEnqueueTaskResponse
      ]

instance FromJSON EnqueueTaskResponse where
  parseJSON (Object v) =
    EnqueueTaskResponse
      <$> v .: "taskId"

-- Query body shape:
--
-- {
--   "task": {
--     "eventName": "exportBlock",
--     "request": {
--       "blockId": "00000000-0000-0000-0000-000000000000",
--       "recursive": false,
--       "exportOptions": {
--         "exportType": "markdown",
--         "timeZone": "Europe/Paris",
--         "locale": "en"
--       }
--     }
--   }
-- }
buildEnqueueTaskBody :: String -> LC.ByteString
buildEnqueueTaskBody pageBlockId =
  encode $
    object
      [ "task"
          .= object
            [ "eventName" .= ("exportBlock" :: String),
              "request"
                .= object
                  [ "blockId" .= (pageBlockId),
                    "recursive" .= (False),
                    "exportOptions"
                      .= object
                        [ "exportType" .= ("markdown" :: String),
                          "timeZone" .= ("Europe/Paris" :: String),
                          "locale" .= ("en" :: String)
                        ]
                  ]
            ]
      ]

requestEnqueueTask :: MonadIO m => String -> String -> m (Response L.ByteString)
requestEnqueueTask token pageBlockId = httpLBS request
  where
    request = buildNotionRequest (BC.pack token) enqueueTaskEndpoint $ buildEnqueueTaskBody pageBlockId
