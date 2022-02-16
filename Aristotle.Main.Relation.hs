{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : Aristotle.Main.Relation   
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
import AristotleRelation  

-- | Location of the local copy of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "relation.page001.Fixed.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom 

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 

main :: IO ()
main = do

-- reads and prints file above   
   fR <- getJSONFrom  
   file2aor fR Relation  

-- convert token to correct type and add to Options 
   tokenIn <- getToken 
   let opts1 = opts2 & auth ?~ oauth2Token (s2bs tokenIn) :: Options 

  -- count is about 4! (should be a lot more)
   let limitpage = 999  
-- set filepath 
   let fp = "fullAristotle" 

   gaaiR <- web2aor opts1 limitpage  
   print (aor_AristotleProcessResults gaaiR) 
   writeAristotleR gaaiR fp 
