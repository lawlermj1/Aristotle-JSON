{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : Aristotle.Main.Issue   
   Description : This is the main module. It reads the Aristotle API, parses the JSON, and exports into CSV.   
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com 

   Use this data to extract UUIDs for Steward Org.    
   
 -}

import qualified Data.ByteString.Lazy as B 
import Data.Csv (encodeDefaultOrderedByName)
import Data.List (nub) 
import Network.Wreq (auth, oauth2Token, Options ) 
import Control.Lens ( (?~), (&) ) 
import qualified Data.ByteString.Internal as I 

import CassavaUtils 
import AristotleCommon
import AristotleIssue  

-- | Location of the local copy of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "issue.page001.Fixed.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom 

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 

main :: IO ()
main = do

-- reads and prints file above   
   fIS <- getJSONFrom  
   file2aoi fIS Issue  

-- convert token to correct type and add to Options 
   tokenIn <- getToken 
   let opts1 = opts2 & auth ?~ oauth2Token (s2bs tokenIn) :: Options 

  -- count is about 173, but with ill formed JSON, many errors 
   let limitpage = 999  
-- set filepath 
   let fp = "fullAristotle" 

   gaaiIS <- web2aoi opts1 limitpage  
   print (aoi_AristotleProcessResults gaaiIS) 
   writeAristotleI gaaiIS fp 
