{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : Aristotle.Main.ObjectClassSpecialisation   
   Description : This is the main module. It reads a single Aristotle API, parses the JSON, and exports into CSV.   
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
import AristotleObjectClassSpecialisation  


-- | Location of the local copy of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "objectclassspecialisation.page001.Fixed.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom 

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 

main :: IO ()
main = do

-- reads and prints file above   
   fOCS <- getJSONFrom  
   file2aoo fOCS ObjectClassSpecialisation 

-- convert token to correct type and add to Options 
   tokenIn <- getToken 
   let opts1 = opts2 & auth ?~ oauth2Token (s2bs tokenIn) :: Options 

  -- set to 9 as low count 
   let limitpage = 9  
-- set filepath 
   let fp = "fullAristotle" 

   gaaiOCS <- web2aoo opts1 limitpage  
   print (aoo_AristotleProcessResults gaaiOCS) 
   writeAristotleOCS gaaiOCS fp 
