{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : Aristotle.Main.Distribution   
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
import AristotleDistribution   


-- | Location of the local copy, in case you have it, of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "distribution.page001.json" 
-- jsonFileFrom = "distribution.page001.Fixed.json"

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 

main :: IO ()
main = do

-- reads file from drive and parses into a csv  
 adBS <- getJSONFrom 
 
 let adFileParse = rb2ad adBS   
 print ("Checking " ++ jsonFileFrom) 
 print (checkParse adFileParse) 

 let adFile = rb2adOut adBS  
 let adFileOut = encodeDefaultOrderedByName adFile 
 B.writeFile "fileAristotleDistribution.csv" adFileOut 

-- convert token to correct type and add to Options 
 tokenIn <- getToken 
 let opts1 = opts2 & auth ?~ oauth2Token (s2bs tokenIn) :: Options 
 
-- reads all pages from Aristotle site and parses into a csv   
 let page1 = "http://dss.aristotlecloud.io/api/v4/metadata/distribution?page=1" 

-- 400 for all pages actual 317 
-- test 10 pages 
 fetched <- crawl opts1 400 (Just page1) [] 

 let parsed = map (rb2ad.fetchedBody) fetched  

 print (nub (map checkParse parsed))  

-- writeFile "fullAristotleDistributionOut.out.txt" (show fetched)  

 let adList = (concat (map (rb2adOut.fetchedBody) fetched))   

 let adOut = encodeDefaultOrderedByName adList 

 B.writeFile "fullAristotleDistribution.csv" adOut   
