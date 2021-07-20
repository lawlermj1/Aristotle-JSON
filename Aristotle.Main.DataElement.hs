{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : Aristotle.Main.DataElement  
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
import AristotleDataElement  


-- Location of the local copy of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "dataElement.page001.json" 
-- jsonFileFrom = "dataElement.page001.Fixed.json" 

-- Read the local copy of the JSON file
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom 

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 


main :: IO ()
main = do

-- reads file from drive and parses into a csv  
 adeBS <- getJSONFrom 
 
 let adeFileParse = rb2ade adeBS   
 print ("Checking " ++ jsonFileFrom) 
 print (checkParse adeFileParse) 

 let adeFile = rb2adeOut adeBS  
 let adeFileOut = encodeDefaultOrderedByName adeFile 
 B.writeFile "fileAristotleDataElement.csv" adeFileOut    

-- convert token to correct type and add to Options 
 tokenIn <- getToken 
 let opts1 = opts2 & auth ?~ oauth2Token (s2bs tokenIn) :: Options 

-- reads all pages from Aristotle site and parses into a csv   
 let page1 = "http://dss.aristotlecloud.io/api/v4/metadata/dataelement?page=1" 

-- full 1000 actual   891 
 fetched <- crawl opts1 1000 (Just page1) [] 

 let parsed = map (rb2ade.fetchedBody) fetched  

 print (nub (map checkParse parsed)) 

-- writeFile "fetched.out" (show fetched)  

 let adeList = (concat (map (rb2adeOut.fetchedBody) fetched))  
 let adeOut = encodeDefaultOrderedByName adeList 

 B.writeFile "fullAristotleDataElement.csv" adeOut   
