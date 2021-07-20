{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : Aristotle.Main.Property   
   Description : This is the main module. It reads the Aristotle API, parses the JSON, and exports into CSV.   
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com 
   
 -}

import qualified Data.ByteString.Lazy as B 
import Data.Csv (encodeDefaultOrderedByName)
import Data.List (nub) 
import Network.Wreq (auth, oauth2Token, Options ) 
import Control.Lens ( (?~), (&) ) 
import qualified Data.ByteString.Internal as I 

import CassavaUtils 
import AristotleCommon
import AristotleProperty  


-- | Location of the local copy, in case you have it, of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "property.page001.json" 
--jsonFileFrom = "property.page001.Fixed.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 

main :: IO ()
main = do

-- reads file from drive and parses into a csv  
 apBS <- getJSONFrom 
 
 let apFileParse = rb2ap apBS   
 print ("Checking " ++ jsonFileFrom) 
 print (checkParse apFileParse) 

 let apFile = rb2apOut apBS  
 let apFileOut = encodeDefaultOrderedByName apFile 

 B.writeFile "fileAristotleProperty.csv" apFileOut 
 
-- convert token to correct type and add to Options 
 tokenIn <- getToken 
 let opts1 = opts2 & auth ?~ oauth2Token (s2bs tokenIn) :: Options 
 
-- reads all pages from Aristotle site and parses into a csv   
 let page1 = "http://dss.aristotlecloud.io/api/v4/metadata/property?page=1" 

-- full 400 actual 306 
 fetched <- crawl opts1 400 (Just page1) [] 

 let parsed = map (rb2ap.fetchedBody) fetched  

 print (nub (map checkParse parsed))  

-- writeFile "fetched.out" (show fetched)  

 let apList = (concat (map (rb2apOut.fetchedBody) fetched))  
 let apOut = encodeDefaultOrderedByName apList 

 B.writeFile "fullAristotleProperty.csv" apOut 
