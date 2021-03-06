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
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GitHub.Endpoints.Users
import GitHub.Endpoints.Repos
import GitHub.Endpoints.Repos.Collaborators
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
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
 -- :<|> getData

crawl :: Maybe String -> Maybe Int -> Handler Bool
crawl Nothing _ = return False
crawl _ Nothing = return False
crawl (Just token) (Just iters) = do
  usr <- liftIO $ userInfoCurrent' (OAuth $ BS.pack token)
  liftIO $ print usr
  case usr of
    Left _ -> return False
    Right u -> do
      liftIO $ recurseOnUser (OAuth $ BS.pack token) (userLogin u) "" iters
      
      return True
{-
getData :: Handler String
getData = do
-} 

recurseOnUser :: Auth -> Name User -> Text -> Int -> IO ()
recurseOnUser _ _ _ 0 = return ()
recurseOnUser t uName rFullName iters = do
  --alreadySeen <- runNeo "MATCH (u:User {name:{uname}}) RETURN u" (fromList [("uname", B.T (untagName uName))])
  print $ "### ITERS : " ++ show iters ++ " ###"
  let alreadySeen = []
  case alreadySeen of
    [] -> do
      print $ "Crawling all over " ++ (unpack $ untagName uName)
      uInfo <- userInfoFor' (Just t) uName
      case uInfo of
        Left e -> return ()
        Right uI -> do
          --let s = makeMapString uI
          --print s
          case rFullName of
            "" -> runNeo (pack ("MERGE (u:User {name:{uname}}) \
                                \ RETURN u ")) (fromList [("uname", B.T ( untagName uName))])
            _ -> runNeo (pack (" MATCH (r:Repo {name:{rname}}) \
                        \ MERGE (u:User {name:{uname}}) \
                        \ MERGE (u)-[:CONTRIBS]->(r)\
                        \ RETURN u")) (fromList [("rname", B.T rFullName), ("uname", B.T (untagName uName))])
          eRepos <- userRepos' (Just t) (mkName Owner (untagName uName)) RepoPublicityAll
          case eRepos of
            Left e -> print e >> return ()
            Right repos -> do
              --print $ (show (untagName uName)) ++ show repos
              --mapM_ (\r -> addRepo (repoName r) (simpleOwnerLogin $ repoOwner r) (uName)) repos
              mapM_ (\r -> recurseOnRepo t (repoName r) (simpleOwnerLogin $ repoOwner r) uName (iters - 1) ) repos
    _ -> print $ "User already seen: " ++ unpack (untagName uName)

instance ToJSON User
instance ToJSON OwnerType

makeMapString :: User -> String
makeMapString (User uid login name typ createdAt pGists avUrl fers fing h bl bio pubRep loc comp email url htmlurl) = 
  "createdAt:\"" ++ show createdAt ++ "\""
    ++ ", followers:\"" ++ show fers ++ "\""
    ++ ", following:\"" ++ show fing ++ "\""
    ++ (case h of
          Just b -> ", hireable:\"" ++ ks (show  b) ++ "\""
          Nothing -> ""
       )
    ++ case bl of
         Just b -> ", blog:" ++ ks (show  b) ++ ""
         Nothing -> ""
    ++ case bio of
         Just b -> ", bio:" ++ ks (show b) ++ ""
         Nothing -> ""
    ++ ", pubRepos:\"" ++ ks (show pubRep) ++ "\""
    ++ case loc of
         Just b -> ", location:" ++ ks (show b) ++ ""
         Nothing -> ""
    ++ case comp of
         Just b -> ", company:" ++ ks (show b) ++ ""
         Nothing -> ""
    ++ case email of
         Just b -> ", email:" ++ ks (show b) ++ ""
         Nothing -> ""
    ++ ", url:\"" ++ ks (unpack (getUrl url)) ++ "\""

-- neo4j kept complaining about backslashes in people's company names etc.
ks :: String -> String
ks = Prelude.map (\c -> if c=='\\' then '.' else c)

kq :: String -> String
kq s = Prelude.map (\c -> if c=='\"' then '.' else c) s


recurseOnRepo :: Auth -> Name Repo -> Name Owner -> Name User -> Int -> IO ()
recurseOnRepo _ _ _ _ 0 = return ()
recurseOnRepo t rName oName uName iters = do
  let fullname = (untagName oName) `append` "/" `append` (untagName rName)
  alreadySeen <- runNeo "MATCH (r:Repo {name:{fullname}}) RETURN r" (fromList [("fullname", B.T fullname)])
  case alreadySeen of
    [] -> do
      print $ "Adding repo " ++ unpack fullname
      eUsers <- contributors' (Just t) (oName) (rName)
      case eUsers of
        Left e -> print e >> return ()
        Right users -> do
          let us = V.mapMaybe contributorToSimpleUser users 
          --V.mapM_ (\u -> addUser (simpleUserLogin u)) us
          runNeo "CREATE (r:Repo {name:{rname}}) RETURN r" (fromList [("rname", B.T fullname)])
          lang' <- languagesFor' (Just t) oName rName
          case lang' of
            Left e -> print e >> return ()
            Right ls -> do
              let lslist = Prelude.map (\l -> B.T (getLanguage l)) (HM.keys ls)
              --print $ (show fullname) ++ " : " ++ (show lslist)
              {-runNeo "MATCH (r:Repo {name:{rname}}) \
                     \ FOREACH (lang in {langs} | \
                     \ MERGE (l:Language {name:lang}) \
                     \ CREATE (r)-[:USES]->(l)) \
                     \ RETURN r " (fromList [("rname", B.T fullname), ("langs", B.L lslist)])
              -}
              runNeo "FOREACH (lang in {langs} | \
                     \ MERGE (l:Language {name:lang}) \
                     \ FOREACH (lang1 in{langs} | \
                     \   MERGE (l1:Language {name:lang1}) \
                     \   MERGE (l)-[r:WITH]-(l1) \
                     \   ON CREATE SET r.num = 1 \
                     \   ON MATCH SET r.num = r.num + 1 \
                     \ )) \
                     \  " (fromList [("rname", B.T fullname), ("langs", B.L lslist)])
              
              return ()
              
            --mapM_ (\u -> addRepo t rName oName (simpleUserLogin u)) us
          --print "after map"
          V.mapM_ (\u -> do
                  case False {-(simpleUserLogin u) == uName -} of
                    True -> return ()
                    False -> recurseOnUser t (simpleUserLogin u) fullname (iters-1)) us
    _ -> print $ "Repo already seen: " ++ (unpack fullname)

addRepo :: Auth -> Name Repo -> Name Owner -> Name User -> IO ()
addRepo t rName oName uName = do
  --print rName
  let n = (untagName oName) `append` "/" `append` (untagName rName)
  let u = untagName uName
  --print $ "Adding Repo: " ++ show n
  result <- runNeo  " CREATE (n:Repo {name:{name}}) \
                    \ MERGE (u:User {name:{uname}}) \
                    \ CREATE (u) -[:CONTRIBS]-> (n) \
                    \ Return n" (fromList [("name", B.T n), ("uname", B.T u)])
  lang' <- languagesFor' (Just t) oName rName
  case lang' of
    Left e -> print e >> return ()
    Right ls -> do
      let lslist = HM.keys ls
      mapM_ (\lang -> do
                runNeo " MATCH (r:Repo {name:{rname}}) \
                       \ MERGE (l:Language {name:{lname}}) \
                       \ CREATE (r)-[:USES]-> (l) \
                       \ RETURN l" (fromList [("lname", B.T (getLanguage lang)), ("rname", B.T n)]))
        lslist
      return ()
        
  --print "DONE"
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
