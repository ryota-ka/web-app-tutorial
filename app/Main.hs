{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON)
import Data.IORef
import Data.List (find)
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

data Error = Error { errorMessage :: T.Text } deriving (Generic, Show)

instance ToJSON Error

type AppState = ([Task], [User])

defaultTasks :: [Task]
defaultTasks = [
    Task 1 "Haskellの勉強会を探す"
  , Task 2 "CAMPHOR- BASEの場所を調べる"
  , Task 3 "Haskellの文法を勉強する"
  , Task 4 "ScottyでWebアプリケーションを作る"
  ]

defaultUsers :: [User]
defaultUsers = [
    User "alice"   "password"
  , User "bob"     "12345678"
  , User "charlie" "qwerty"
  ]

addTask :: IORef [Task] -> T.Text -> IO Task
addTask ref title = atomicModifyIORef' ref transform
  where
    transform :: [Task] -> ([Task], Task)
    transform tasks =
      let newTask = Task (length tasks + 1) title
          in (newTask:tasks, newTask)

currentUser :: ActionM (Maybe User)
currentUser = do
  maybeUsername <- safeParam "username"
  maybePassword <- safeParam "password"
  let maybeUser =
        case (maybeUsername, maybePassword) of
             (Just u, Just p) -> find (== User u p) defaultUsers
             (_, _) -> Nothing
  return maybeUser

safeParam :: Parsable a => T.Text -> ActionM (Maybe a)
safeParam key = do
  params' <- params
  let value = snd <$> find ((== key) . fst) params'
  let parsedValue = case value of
                         Nothing -> Nothing
                         Just value -> case parseParam value of
                                            Left _ -> Nothing
                                            Right v -> Just v
  return parsedValue

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

    post "/login" $ do
      maybeUser <- currentUser
      case maybeUser of
           Nothing -> status status401 >> text "authorization required"
           Just user -> text $ "Hello, " <> username user <> "!"

    get "/tasks" $ do
      tasks <- liftIO $ readIORef ref
      json tasks

    post "/tasks" $ do
      title <- param "title"
      newTask <- liftIO $ addTask ref title
      status status201
      json newTask
