{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON)
import Data.IORef
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

type Username = T.Text
type Password = T.Text

data User = User {
    username :: Username
  , password :: Password
} deriving (Eq, Generic, Show)

instance ToJSON User

defaultTasks :: [Task]
defaultTasks = [
    Task 1 "Haskellの勉強会を探す"
  , Task 2 "CAMPHOR- BASEの場所を調べる"
  , Task 3 "Haskellの文法を勉強する"
  , Task 4 "ScottyでWebアプリケーションを作る"
  ]

addTask :: IORef [Task] -> T.Text -> IO Task
addTask ref title = atomicModifyIORef' ref transform
  where
    transform :: [Task] -> ([Task], Task)
    transform tasks =
      let newTask = Task (length tasks + 1) title
          in (newTask:tasks, newTask)

main :: IO ()
main = do
  ref <- newIORef defaultTasks

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
      tasks <- liftIO $ readIORef ref
      json tasks

    post "/tasks" $ do
      title <- param "title"
      newTask <- liftIO $ addTask ref title
      status status201
      json newTask
