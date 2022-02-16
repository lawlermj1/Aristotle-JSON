{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : Aristotle.Main.ConceptDelta   
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
import AristotleConceptDelta  

-- | Location of the local copy of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "conceptdelta.page001.Fixed.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom 

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 

main :: IO ()
main = do

-- reads and prints file above   
   fCD <- getJSONFrom  
   file2aoc fCD ConceptDelta  

-- convert token to correct type and add to Options 
   tokenIn <- getToken 
   let opts1 = opts2 & auth ?~ oauth2Token (s2bs tokenIn) :: Options 

  -- count is about 52,000 so 99999 
  -- test set to 2 
   let limitpage = 2    
-- set filepath 
   let fp = "fullAristotle" 

   gaaiCD <- web2aoc opts1 limitpage  
   print (aoc_AristotleProcessResults gaaiCD) 

--   print (aoc_AristotleConceptDeltaIn gaaiCD) 

   writeAristotleCD gaaiCD fp 

-- Create these gets using anyitem 
-- "https://aristotle.cloud/api/v4/metadata/ffff7f36-dc6d-11ea-a913-0a6933dafb30"
-- -> 
-- {
--    "id": 36879,
--    "created": "2020-08-12T17:32:39.270118+10:00",
-- ...

   sf <- fetchUUID opts1 "ffff7f36-dc6d-11ea-a913-0a6933dafb30"  
   print sf 
