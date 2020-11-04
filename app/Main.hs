{-# LANGUAGE  OverloadedStrings #-}
module Main where

import Types
import JSON
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import ServantModule
main :: IO ()
main = do
	
	let conn = Connection "input.in"
	x <- findStudent 1 conn
	print x
	--removeStudent 1 conn
	putStrLn("finish")
        runServer 
