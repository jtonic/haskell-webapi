{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}


module Web.Server where

import           Prelude                    (Bool (True), Show (show), print,
                                             putStrLn, ($))
import           Prelude.Compat             (IO, Monad (return))

import           Control.Monad              (mapM_)
import           Control.Monad.Except       (MonadIO (liftIO))
import           Control.Monad.Extra        (unlessM)
import           Control.Monad.Reader       ()
import           Data.Attoparsec.ByteString ()
import qualified Data.ByteString.Char8      as BSC
import           Data.List                  ()
import           Data.Maybe                 ()
import           Data.String.Conversions    ()
import           Data.String.Interpolate    (__i'L, i)
import qualified Network.Wai.Handler.Warp   as W
import           Persistence.Ddl            (initDB)
import           Persistence.Dml            (getMessages, saveMessage)
import           Servant
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist)
import           System.FilePath
import           Web.Data                   (users)
import           Web.Types

type UserAPI = "users" :> Get '[JSON] [User]
             :<|> "users" :> ReqBody '[JSON] User
                         :> Post '[JSON] NoContent


newUser :: User -> Handler NoContent
newUser u = do
    liftIO $ putStrLn [__i'L|
        Create user:
        #{show u}
    |]
    return NoContent


server :: Server UserAPI
server = getUsers
    :<|> newUser
    where
        getUsers :: Handler [User]
        getUsers = return users


userAPI :: Proxy UserAPI
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

run :: IO ()
run = do
    dbFile <- getOrCreateDbFile defaultDbPath
    initDB dbFile
    saveMessage dbFile "This is it!"
    msgs <- getMessages dbFile
    mapM_ print msgs
    BSC.putStrLn [i|Starting the HTTP server on port #{port}...|]
    W.run port app1

    where
        port = 8080
        defaultDbPath = "data/test.db" :: FilePath
        getOrCreateDbFile :: FilePath -> IO FilePath
        getOrCreateDbFile filePath = do
                unlessM (doesFileExist filePath) (createDirectoryIfMissing True $ takeDirectory filePath)
                return filePath


users' :: [User]
users' = users

