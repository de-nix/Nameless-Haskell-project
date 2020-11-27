{-# LANGUAGE  OverloadedStrings #-}

module Main where

import Types
import JSON
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import ServantModule

main :: IO ()
main = do
    runServer 
