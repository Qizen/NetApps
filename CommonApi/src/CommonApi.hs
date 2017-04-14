{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module CommonApi where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

{-
data DBData = DBData
  { nodes :: [Node]
  , links :: [Link]
  }

data Node = Node
  { id :: String
  , labels :: [String]
  , properties :: [Properties]
  }

data Properties = Properties
  { key :: String
  , val :: String
  }

data Link = Link
  {
-}
    
type API = "crawl" :> QueryParam "token" String :> QueryParam "iters" Int :> Get '[JSON] Bool
--   :<|> "getData" :> Get '[JSON] String

