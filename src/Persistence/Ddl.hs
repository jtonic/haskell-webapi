{-# LANGUAGE OverloadedStrings #-}

module Persistence.Ddl where

import           Database.SQLite.Simple (execute_, withConnection)

initDB :: FilePath -> IO ()
initDB dbFile = withConnection dbFile $ \conn ->
    execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"
