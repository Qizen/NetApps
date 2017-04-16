module Handler.Crawl where

import Import
import GitHub.Endpoints.Users
import qualified CommonApi as CA
import qualified Servant as S
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)

numIter = 15
crawlerIp = "127.0.0.1"
crawlerPort = 23455

postCrawlR :: Handler Html
postCrawlR = do
  (_,user) <- requireAuthPair
  defaultLayout $ do
    Just nm <- lookupSession "login"
    Just token <- lookupSession "access_token"
    liftIO $ callCrawler (unpack token) numIter


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
