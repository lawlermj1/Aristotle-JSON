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

-- Just ObjectCalss and Property for grammar parsing 
-- DataElementConcept for sample name format checking 

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

 let gaaiList = [gaaiOC , gaaiP, gaaiDEC] 

 -}

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as DC 
import qualified Data.Data as DD 
import Network.Wreq (auth, oauth2Token, Options ) 
import Control.Lens ( (?~), (&) ) 

import CassavaUtils 
import AristotleCommon
import AristotleAnyItem  
import PartOfSpeech 
-- (SystemName)

-- data SystemName = Aristotle | Sparx deriving ( Eq, Ord, DD.Typeable, Show, Read, Enum )  

-- | Location of the local copy of the JSON file.
jsonFileFrom :: FilePath
jsonFileFrom = "anyitem.page001.Fixed.json" 

-- Read the local copy of the JSON file.
getJSONFrom :: IO BL.ByteString
getJSONFrom = BL.readFile jsonFileFrom 

-- read the Aristotle provided token 
getToken :: IO String 
getToken = readFile "token.txt" 

--definedName :: !String , definedSpace :: !String     , definedIn :: !SystemName    
-- ai2on_AnyItemType :: String , ai2on_name :: String 
--
--a4n2ns :: SystemName -> AristotleAnyItemName -> Name2Namespace
--a4n2ns sn ain = Name2Namespace (ai2on_name ain) (ai2on_AnyItemType ain) sn 
a4n2ns :: AristotleAnyItemName -> Name2Namespace
a4n2ns ain = Name2Namespace (ai2on_name ain) (ai2on_AnyItemType ain) (definedIn defaultName2Namespace)    

-- word2Map :: [Words2POS] -> M.Map String Words2POS  
--n2ns4n2ws :: M.Map String Words2POS -> [Name2Namespace] -> [Name2Words] 
--n2ns4n2ws wlu n2nsL = map (\o -> nm2ws wlu o) n2nsL 

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

 gaaiOC <- web2aoa opts1 limitpage pagesize ObjectClass   
 print (aoa_AristotleProcessResults gaaiOC) 
 print (aoa_AristotleTestProperty gaaiOC) 
 print (aoa_AristotleParseResult gaaiOC) 
 print "----------------------"  

 gaaiP <- web2aoa opts1 limitpage pagesize Property   
 print (aoa_AristotleProcessResults gaaiP)  
 print (aoa_AristotleTestProperty gaaiP) 
 print (aoa_AristotleParseResult gaaiP) 
 print "----------------------"  

-- collect gaaiOCS fp 
 let gaaiList = [gaaiOC, gaaiP] 

 let gaaiAll = concatAristotleOutAll gaaiList 

 let ocName = map anyItemType2Name (aoa_AristotleAnyItemOut gaaiOC) 
 let pName = map anyItemType2Name (aoa_AristotleAnyItemOut gaaiP)  
-- print ocName 

-- writeAristotle gaaiAll fp 

 w2posFile <- BL.readFile "W2POS.csv" 
 let err = (decodeByNameEither w2posFile) :: Either String [W2POS] 
--    print err  
 let ws = (decodeByNameOK w2posFile) :: [W2POS] 
--    print (take 50 ws)

 let w2pos = cw2pSortAll ws 
--    print w2pos  
 BL.writeFile "w2posOut.csv" (DC.encodeDefaultOrderedByName w2pos)      
--    print (take 50 w2pos) 

 let wss = sortWords2POS ws 
--    BL.writeFile "WSS.csv" (DC.encodeDefaultOrderedByName wss)   
--    print (take 5 wss) 

--    let wssFilter = map snd (singleWords2POS wss)  
--    BL.writeFile "WSSFilter.csv" (DC.encodeDefaultOrderedByName wssFilter)   
--    print (take 5 wss) 

--    let w2posLookup = word2Map wssFilter  
 let w2posLookup = word2Map (map snd (singleWords2POS wss))        
 let w2posLookupOut = toMapW2POS w2posLookup 
 BL.writeFile "w2posLookupOut.csv" (DC.encodeDefaultOrderedByName w2posLookupOut)  

-- n2ns4n2ws :: M.Map String Words2POS -> [Name2Namespace] -> [Name2Words] 
 let name2WordsOC = n2ns4n2ws w2posLookup (map a4n2ns ocName) 
--n2ws4s2wOutAll :: [Name2Words] -> [Space2WordsOut] 
--    let space2WordsOut = n2ws4s2wOutAll name2WordsOC 
 BL.writeFile "name2WordsOC.csv" (DC.encodeDefaultOrderedByName name2WordsOC)   

 let space2WordsOC = n2ws4s2ws name2WordsOC 
 let space2WordsFreqOutOC = s2wfsOut w2posLookup space2WordsOC 
 BL.writeFile "space2WordsFreqOutOC.csv" (DC.encodeDefaultOrderedByName space2WordsFreqOutOC) 

-- Property 
 let name2WordsP = n2ns4n2ws w2posLookup (map a4n2ns pName) 
 BL.writeFile "name2WordsP.csv" (DC.encodeDefaultOrderedByName name2WordsP)   

 let space2WordsP = n2ws4s2ws name2WordsP 
 let space2WordsFreqOutP = s2wfsOut w2posLookup space2WordsP 
 BL.writeFile "space2WordsFreqOutP.csv" (DC.encodeDefaultOrderedByName space2WordsFreqOutP) 
