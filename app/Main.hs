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
  , user   :: User
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
    Task 1 "Haskellの勉強会を探す" alice
  , Task 2 "CAMPHOR- BASEの場所を調べる" alice
  , Task 3 "Haskellの文法を勉強する" alice
  , Task 4 "ScottyでWebアプリケーションを作る" alice
  ]

defaultUsers :: [User]
defaultUsers = [
    User "alice"   "password"
  , User "bob"     "12345678"
  , User "charlie" "qwerty"
  ]

alice :: User
alice = head defaultUsers

addTask :: IORef AppState -> T.Text -> User -> IO Task
addTask ref title user = atomicModifyIORef' ref transform
  where
    transform :: AppState -> (AppState, Task)
    transform (tasks, users) =
      let newTask = Task (length tasks + 1) title user
          in ((newTask:tasks, users), newTask)

addUser :: IORef AppState -> User -> IO User
addUser ref user = atomicModifyIORef' ref transform
  where
    transform :: AppState -> (AppState, User)
    transform (tasks, users) = ((tasks, user:users), user)

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
  ref <- newIORef (defaultTasks, defaultUsers)

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
      maybeUser <- currentUser
      case maybeUser of
           Nothing -> status status401 >> text "unauthorized"
           Just authenticatedUser -> do
             tasks <- fst <$> (liftIO $ readIORef ref)
             let ownTasks = filter ((== authenticatedUser) . user) tasks
             json tasks

    post "/tasks" $ do
      maybeUser <- currentUser
      case maybeUser of
           Nothing -> status status401 >> text "unauthorized"
           Just authenticatedUser -> do
             title <- param "title"
             newTask <- liftIO $ addTask ref title authenticatedUser
             status status201
             json newTask

    post "/users" $ do
      maybeUser <- currentUser
      case maybeUser of
           Nothing -> status status401 >> json (Error "username and password required")
           Just user -> do
             newUser <- liftIO $ addUser ref user
             status status201
             json newUser
