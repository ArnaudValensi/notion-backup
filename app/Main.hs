{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main where

import ExportPageCommand (exportPageCommand)
import Options.Generic

data Args = Args {apiToken :: String, pageId :: String} deriving (Generic, Show)

instance ParseRecord Args

main :: IO ()
main = do
  args <- getRecord "Notion Backup"
  exportPageCommand (apiToken args) (pageId args)
