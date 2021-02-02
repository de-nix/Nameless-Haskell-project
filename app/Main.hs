{-# LANGUAGE  OverloadedStrings #-}

module Main where

import Types
import JSON
import Data.Aeson
import ServantModule

main :: IO ()
main = do
    putStrLn("hello world")
    runServer
