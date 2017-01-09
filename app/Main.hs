{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.IORef
import Data.Monoid
import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 $ (spock spockCfg) app

app :: SpockM () () () ()
app = do
  get root $ do
    text "Hello, World!"

  get ("hello" <//> var) $ \name -> do
    text $ "Hello, " <> name <> "!"
