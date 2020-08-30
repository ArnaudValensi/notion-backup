{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Utils
  ( buildNotionRequest,
    processResponse,
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

apiHost :: BC.ByteString
apiHost = "www.notion.so"

headers :: BC.ByteString -> [Header]
headers notionToken =
  [ ("authority", "www.notion.so"),
    ("pragma", "no-cache"),
    ("cache-control", "no-cache"),
    ("content-type", "application/json"),
    ("user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/84.0.4147.135 Safari/537.36"),
    ("notion-client-version", "23.2.10"),
    ("accept", "*/*"),
    ("origin", "https://www.notion.so"),
    ("sec-fetch-site", "same-origin"),
    ("sec-fetch-mode", "cors"),
    ("sec-fetch-dest", "empty"),
    ("referer", "https://www.notion.so/Test-one-block-ca0c8c0fbfc34052a17835fbffccfce5"),
    ("accept-language", "fr-FR,fr;q=0.9,en-US;q=0.8,en;q=0.7"),
    ("cookie", "token_v2=" <> notionToken)
  ]

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> LC.ByteString -> Request
buildRequest token host method path requestBody =
  setRequestMethod method $
    setRequestHost host $
      setRequestHeaders (headers token) $
        setRequestPath path $
          setRequestSecure True $
            setRequestPort 443 $
              setRequestBodyLBS requestBody $
                defaultRequest

buildNotionRequest :: BC.ByteString -> BC.ByteString -> LC.ByteString -> Request
buildNotionRequest notionToken path body = buildRequest notionToken apiHost "POST" path body

processResponse :: Response L.ByteString -> Either String L.ByteString
processResponse response =
  if status == 200
    then Right jsonBody
    else Left $ "Error " <> statusText <> ": " <> show jsonBody
  where
    status = getResponseStatusCode response
    jsonBody = getResponseBody response
    statusText = show $ status
