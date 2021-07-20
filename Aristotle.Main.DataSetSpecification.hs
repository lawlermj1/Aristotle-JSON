{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : Aristotle.Main.DataSetSpecification   
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
import AristotleDataSetSpecification    


-- | Location of the local copy, in case you have it, of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "datasetspecification.page001.Fixed.json" 
--jsonFileFrom = "datasetspecification.page001.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 

main :: IO ()
main = do

-- reads file from drive and parses into a csv  
 adssBS <- getJSONFrom 
 
 let adssFileParse = rb2adss adssBS   
 print ("Checking " ++ jsonFileFrom) 
 print (checkParse adssFileParse) 

 let adssFile = rb2adssOut adssBS  
 let adssFileOut = encodeDefaultOrderedByName adssFile 
 B.writeFile "fileAristotleDataSetSpecification.csv" adssFileOut 

-- convert token to correct type and add to Options 
 tokenIn <- getToken 
 let opts1 = opts2 & auth ?~ oauth2Token (s2bs tokenIn) :: Options 

-- reads all pages from Aristotle site and parses into a csv   
 let page1 = "http://dss.aristotlecloud.io/api/v4/metadata/datasetspecification?page=1" 

-- max 99 actual 
 fetched <- crawl opts1 99 (Just page1) [] 

 let parsed = map (rb2adss.fetchedBody) fetched  

 print (nub (map checkParse parsed))  

-- writeFile "fetched.out" (show fetched)  

 let adssList = (concat (map (rb2adssOut.fetchedBody) fetched))  
 let adssOut = encodeDefaultOrderedByName adssList 

 B.writeFile "fullAristotleDataSetSpecification.csv" adssOut   
