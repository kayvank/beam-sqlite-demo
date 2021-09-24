{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Q2.Web.Api where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import           Data.Proxy
import qualified Data.Text                as T
import           Data.Time                (UTCTime)
import           GHC.Generics
import qualified MocData                  as Moc
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Q2.Types                 as Q2
import           Servant
import           Servant.API
import           Servant.Client
import           Servant.Server
import           Servant.Types.SourceT    (foreach)

type UserApi =
  "users" :> Get '[JSON] [Q2.User]
    :<|> "user" :> Capture "id" String :> Get '[JSON] Q2.User
    -- :<|> "user" :> Capture "username" String :> DeleteNoContent
    -- :<|> "user" :> Capture "username" String :> Put '[JSON] Q2.User
    -- :<|> "user" :> ReqBody '[JSON] Q2.User

data SortBy = Age | Name

userApi :: Proxy UserApi
userApi = Proxy

getUsers ::  Handler [Q2.User]
getUsers =  pure $  Moc.buildUsers ids where
  ids = ["id-1","id-2", "id-3" ] :: [T.Text]   -- TODO fix me

getUser :: String -> Handler Q2.User
getUser  id = pure $ head $ Moc.buildUsers ids where
  ids = [T.pack id] :: [T.Text]   -- TODO fix me

myServer :: Server UserApi
myServer = getUsers :<|> getUser

userApp :: Application
userApp  = serve userApi myServer

runServer :: IO ()
runServer = run 9000  userApp
