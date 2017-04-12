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
    Right u -> liftIO $ recurseOnUser (OAuth $ BS.pack token) (userLogin u) iters >> return True
  return True

recurseOnUser :: Auth -> Name User -> Int -> IO ()
recurseOnUser _ _ 0 = return ()
recurseOnUser t uName iters = do
  eRepos <- userRepos' (Just t) (mkName Owner (untagName uName)) RepoPublicityPublic
  case eRepos of
    Left e -> print e >> return ()
    Right repos -> do
      mapM_ (\r -> addRepo (repoName r) (simpleOwnerLogin $ repoOwner r)) repos
      mapM_ (\r -> recurseOnRepo t (repoName r) (simpleOwnerLogin $ repoOwner r) (iters - 1) ) repos

addRepo :: Name Repo -> Name Owner -> IO ()
addRepo rName oName = print rName

recurseOnRepo :: Auth -> Name Repo -> Name Owner -> Int -> IO ()
recurseOnRepo _ _ _ 0 = return ()
recurseOnRepo t rName oName iters = do
  eUsers <- contributors' (Just t) (oName) (rName)
  case eUsers of
    Left e-> print e >> return ()
    Right users -> do
      let us = V.mapMaybe contributorToSimpleUser users 
      V.mapM_ (\u -> addUser (simpleUserLogin u)) us
      V.mapM_ (\u -> recurseOnUser t (simpleUserLogin u) (iters-1)) us

addUser :: Name User -> IO ()
addUser name = print name
