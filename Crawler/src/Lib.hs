{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings#-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Map
import Data.Maybe
import qualified Data.Vector as V
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GitHub.Endpoints.Users
import GitHub.Endpoints.Repos
import GitHub.Endpoints.Repos.Collaborators
import qualified Data.ByteString.Char8 as BS
import GHC.Generics
import System.IO
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Text
import qualified Database.Bolt as B

import CommonApi

startApp :: IO ()
startApp = run 23455 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = crawl

crawl :: Maybe String -> Maybe Int -> Handler Bool
crawl Nothing _ = return False
crawl _ Nothing = return False
crawl (Just token) (Just iters) = do
  usr <- liftIO $ userInfoCurrent' (OAuth $ BS.pack token)
  liftIO $ print usr
  case usr of
    Left _ -> return False
    Right u -> do
      liftIO $ recurseOnUser (OAuth $ BS.pack token) (userLogin u) iters
      
      return True

recurseOnUser :: Auth -> Name User -> Int -> IO ()
recurseOnUser _ _ 0 = return ()
recurseOnUser t uName iters = do
  print $ "Crawling all over " ++ (unpack $ untagName uName)
  runNeo "MERGE (u:User {name:{uname}}) RETURN u" (fromList [("uname", B.T (untagName uName))])
  eRepos <- userRepos' (Just t) (mkName Owner (untagName uName)) RepoPublicityPublic
  case eRepos of
    Left e -> print e >> return ()
    Right repos -> do
      --print repos
      --mapM_ (\r -> addRepo (repoName r) (simpleOwnerLogin $ repoOwner r) (uName)) repos
      mapM_ (\r -> recurseOnRepo t (repoName r) (simpleOwnerLogin $ repoOwner r) uName (iters - 1) ) repos

recurseOnRepo :: Auth -> Name Repo -> Name Owner -> Name User -> Int -> IO ()
recurseOnRepo _ _ _ _ 0 = return ()
recurseOnRepo t rName oName uName iters = do
  eUsers <- contributors' (Just t) (oName) (rName)
  case eUsers of
    Left e -> print e >> return ()
    Right users -> do
      let us = V.mapMaybe contributorToSimpleUser users 
      --V.mapM_ (\u -> addUser (simpleUserLogin u)) us
      mapM_ (\u -> addRepo rName oName (simpleUserLogin u)) us
      print "after map"
      V.mapM_ (\u -> do
                  case (simpleUserLogin u) == uName of
                    True -> return ()
                    False -> recurseOnUser t (simpleUserLogin u) (iters-1)) us

addRepo :: Name Repo -> Name Owner -> Name User -> IO ()
addRepo rName oName uName = do
  print rName
  let n = (untagName oName) `append` "/" `append` (untagName rName)
  let u = untagName uName
  print $ "Adding Repo: " ++ show n
  result <- runNeo  " MERGE (u:User {name:{uname}}) \
                    \ MERGE (n:Repo {name:{name}}) \
                    \ MERGE (u) -[:CONTRIBS]-> (n) \
                    \ Return n" (fromList [("name", B.T n), ("uname", B.T u)])
  print "DONE"
  --print $ "res: " ++ show result
  return ()

addUser :: Name User -> IO ()
addUser name = do
  print name

runNeo :: Text -> Map Text B.Value -> IO ([B.Record])
runNeo q m = do
  pipe <-B.connect $ B.def { B.user = "neo4j", B.password = "password" }
  result <- B.run pipe $ B.queryP q m
  B.close pipe
  return result
