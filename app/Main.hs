{-# LANGUAGE  OverloadedStrings #-}
module Main where

import Types
import JSON
import qualified Data.ByteString.Lazy as B
import Data.Aeson
main :: IO ()
main = do
	
	let s1 = Student 1 "Denis" "933" "cdie2513"
            conn = Connection "input.in"
	createStudent s1 (pure conn)
	putStrLn("finish") 
