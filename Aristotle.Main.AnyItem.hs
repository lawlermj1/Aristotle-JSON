{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : Aristotle.Main.AnyItem   
   Description : This is the main module. It reads the Aristotle API, parses the JSON, and exports into CSV.   
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com 

   This is the Boilerplate for all items.
   Use with the page get call. 
   Single get used with ConceptDelta. 

   need to make permanent 
:set -package uuid-types   

-- full set 

 -}

import qualified Data.ByteString.Lazy as B 
import Data.Csv (encodeDefaultOrderedByName)
import Data.List (nub) 
import Network.Wreq (auth, oauth2Token, Options ) 
import Control.Lens ( (?~), (&) ) 
import qualified Data.ByteString.Internal as I 
import Data.Either 
import Data.Maybe  

import CassavaUtils 
import AristotleCommon
import AristotleAnyItem  
import PartOfSpeech 

-- | Location of the local copy of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "anyitem.page001.Fixed.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO B.ByteString
getJSONFrom = B.readFile jsonFileFrom 

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 


main :: IO ()
main = do

-- convert token to correct type and add to Options 
 tokenIn <- getToken 
 let opts1 = opts2 & auth ?~ oauth2Token (s2bs tokenIn) :: Options 

 -- set to 9 for testing 
 let limitpage = 9999 
-- set to 5 for testing 
 let pagesize = 100  
-- set filepath 
 let fp = "fullAristotle" 

-- get each AristotleOutAll in count order 
-- start 14:53 
-- end   15:24 
-- get time = 31 minutes 

 gaaiDT <- web2aoa opts1 limitpage pagesize DataType   
 print (aoa_AristotleProcessResults gaaiDT) 
 print (aoa_AristotleTestProperty gaaiDT) 
 print (aoa_AristotleParseResult gaaiDT) 
 print "----------------------"

 gaaiOCS <- web2aoa opts1 limitpage pagesize ObjectClassSpecialisation  
 print (aoa_AristotleProcessResults gaaiOCS) 
 print (aoa_AristotleTestProperty gaaiOCS) 
 print (aoa_AristotleParseResult gaaiOCS) 
 print "----------------------"

 gaaiDSS <- web2aoa opts1 limitpage pagesize DataSetSpecification  
 print (aoa_AristotleProcessResults gaaiDSS) 
 print (aoa_AristotleTestProperty gaaiDSS) 
 print (aoa_AristotleParseResult gaaiDSS) 
 print "----------------------" 

 gaaiOC <- web2aoa opts1 limitpage pagesize ObjectClass   
 print (aoa_AristotleProcessResults gaaiOC) 
 print (aoa_AristotleTestProperty gaaiOC) 
 print (aoa_AristotleParseResult gaaiOC) 
 print "----------------------"  

 gaaiR <- web2aoa opts1 limitpage pagesize Relation   
 print (aoa_AristotleProcessResults gaaiR)  
 print (aoa_AristotleTestProperty gaaiR) 
 print (aoa_AristotleParseResult gaaiR) 
 print "----------------------"  

 gaaiVD <- web2aoa opts1 limitpage pagesize ValueDomain   
 print (aoa_AristotleProcessResults gaaiVD) 
 print (aoa_AristotleTestProperty gaaiVD) 
 print (aoa_AristotleParseResult gaaiVD) 
 print "----------------------" 

 gaaiP <- web2aoa opts1 limitpage pagesize Property   
 print (aoa_AristotleProcessResults gaaiP)  
 print (aoa_AristotleTestProperty gaaiP) 
 print (aoa_AristotleParseResult gaaiP) 
 print "----------------------"  

 gaaiDEC <- web2aoa opts1 limitpage pagesize DataElementConcept   
 print (aoa_AristotleProcessResults gaaiDEC)  
 print (aoa_AristotleTestProperty gaaiDEC) 
 print (aoa_AristotleParseResult gaaiDEC) 
 print ""

 gaaiD <- web2aoa opts1 limitpage pagesize Distribution   
 print (aoa_AristotleProcessResults gaaiD)  
 print (aoa_AristotleTestProperty gaaiD) 
 print (aoa_AristotleParseResult gaaiD) 
 print "----------------------"

 gaaiDE <- web2aoa opts1 limitpage pagesize DataElement   
 print (aoa_AristotleProcessResults gaaiDE) 
 print (aoa_AristotleTestProperty gaaiDE) 
 print (aoa_AristotleParseResult gaaiDE) 
 print "----------------------" 

-- collect gaaiOCS fp 
 let gaaiList = [gaaiDT , gaaiOCS, gaaiDSS, gaaiOC, gaaiR, gaaiVD, gaaiP, gaaiDEC, gaaiD, gaaiDE] 
-- let gaaiList = [gaaiDT , gaaiOCS, gaaiD] 

 let gaaiAll = concatAristotleOutAll gaaiList
 writeAristotle gaaiAll fp 
