{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeOperators #-}

module Web.Client where

import           Data.Proxy          (Proxy (..))
import           Data.Text           ()
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API         as S
import           Servant.Client      as S
import           Web.Data            (newton)
import           Web.Types           (User)

type UsersApi = "users" :> Get '[JSON] [User]                                     -- GET /users
              :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] NoContent      -- POST /users

usersApi :: Proxy UsersApi
usersApi = Proxy

postNewUser :: User -> ClientM NoContent
getAllUsers :: ClientM [User]
(getAllUsers :<|> postNewUser) = client usersApi

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  let clientM = mkClientEnv manager' (BaseUrl Http "localhost" 8080 "")
  newUser clientM newton
  getUsers clientM
  where
      getUsers clientM = do
          res <- runClientM getAllUsers clientM
          case res of
            Left err    -> putStrLn $ "Error: " ++ show err
            Right users -> print users
      newUser clientM user = do
          res <- runClientM (postNewUser user) clientM
          case res of
            Left err    -> putStrLn $ "Error: " ++ show err
            Right users -> print users
