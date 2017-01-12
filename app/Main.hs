{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Data.Aeson (ToJSON)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Network.HTTP.Types
import Web.Scotty
import qualified Data.Text.Lazy as T

data Task = Task {
    taskId :: Int
  , title  :: T.Text
  } deriving (Eq, Generic, Show)

instance ToJSON Task

defaultTasks :: [Task]
defaultTasks = [
    Task 1 "Haskellの勉強会を探す"
  , Task 2 "CAMPHOR- BASEの場所を調べる"
  , Task 3 "Haskellの文法を勉強する"
  , Task 4 "ScottyでWebアプリケーションを作る"
  ]

main :: IO ()
main = do
  scotty 8080 $ do
    get "/" $ do
      text "Hello, world!"

    get "/hello/:name" $ do
      name <- param "name"
      text $ "Hello, " <> name <> "!"

    get "/redirect/to/root" $ do
      status status302
      setHeader "X-Foo-Bar" "bazqux"
      redirect "/"

    get "/tasks" $ do
      json defaultTasks
