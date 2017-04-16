module Handler.Profile where

import Import
import GitHub.Endpoints.Users
import qualified CommonApi as CA
import qualified Servant as S
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)

numIter = 10
crawlerIp = "127.0.0.1"
crawlerPort = 23455


getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        Right usr <- liftIO $ userInfoFor "qizen"
        let name = userLogin usr
        Just nm <- lookupSession "login"
        Just token <- lookupSession "access_token"
        Right currUser <- liftIO $ userInfoCurrent' (OAuth $ encodeUtf8 token)
        let createdAt = userCreatedAt currUser
        setTitle . toHtml $ nm <> "'s User Page"
        --liftIO $ callCrawler (unpack token) numIter
        $(widgetFile "profile")

--should in theory branch off to a separate servant endpoint for github crawling.

api :: S.Proxy (CA.API)
api = S.Proxy

crawl :: Maybe String -> Maybe Int -> ClientM Bool

crawl = client api

callCrawler :: String -> Int -> IO ()
callCrawler token iters = do
  manager <- Network.HTTP.Client.newManager Network.HTTP.Client.defaultManagerSettings
  res <- runClientM (crawl (Just token) (Just iters)) (ClientEnv manager (BaseUrl Http crawlerIp crawlerPort "" ))
  print $ "Called Crawler. Result: " ++ show res
  return ()
