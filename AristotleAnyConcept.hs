{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleAnyConcept
   Description : This contains specific JSON and CSV parsing functions for the Aristotle AnyConcept.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/

   The parsing does work, on a single JSON record, because this is the Boilerplate for all items. 
   Get requests are by key not page, so rewrite of is needed. 
   This will be inefficient, as it will be IO bound via IO throttling. 
   It may be needed later for CDC style access, bu it has lower priority. 

   But it is still not fully working yet, as 
   1. the API does not return a paging wrapper with 
 ["Error in $: parsing AristotleAnyConcept.AristotleAnyConceptJSON(AristotleAnyConceptJSON) failed, key \"count\" not found"]
   2. How to get a list of item_uuid to feed into separate calls? 
   Basically manage the paging. 

   No permissions message 
 { "detail": "You do not have permission to perform this action." } 

 -}
module AristotleAnyConcept  
    (  
      
      AristotleOutAC( .. ), 
      
      writeAristotleAC, 
      web2aoa, 
      file2aoa, 

     ) where
    
import Data.Aeson
import Data.Either 
import Data.List 
import Data.Maybe  
import Data.Time  
import GHC.Generics
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered, encodeDefaultOrderedByName)  
import qualified Data.ByteString.Lazy as B 
import qualified Network.Wreq as W 

import CassavaUtils 
import AristotleCommon  

-- Primary type 
data AristotleAnyConcept  = 
  AristotleAnyConcept { 
            aac_id :: Int 
          , aac_uuid :: String
          , aac_name :: String
          , aac_version :: String           
          , aac_definition :: String
          , aac_short_definition :: String  
          , aac_absolute_url :: String 
          , aac_expand_node_get_url :: String
          } deriving (Show,Generic) 

instance ToJSON AristotleAnyConcept where 
    toJSON (AristotleAnyConcept 
              aac_id aac_uuid 
              aac_name aac_definition 
              aac_version aac_short_definition 
              aac_absolute_url aac_expand_node_get_url 
      ) = 
        object ["id" .= aac_id ,"uuid" .= aac_uuid   
               ,"name" .= aac_name ,"version" .= aac_version 
               ,"definition" .= aac_definition 
               ,"short_definition" .= aac_short_definition 
               ,"absolute_url" .= aac_absolute_url      
               ,"expand_node_get_url" .= aac_expand_node_get_url                                                              
               ]

instance FromJSON AristotleAnyConcept where 
    parseJSON = withObject "AristotleAnyConcept" $ \v -> AristotleAnyConcept  
        <$> v .: "id" 
        <*> v .: "uuid" 
        <*> v .: "name" 
        <*> v .: "version"        
        <*> v .: "definition" 
        <*> v .: "short_definition" 
        <*> v .: "absolute_url" 
        <*> v .: "expand_node_get_url" 

instance Eq AristotleAnyConcept where
  (AristotleAnyConcept id1 _ _ _ _ _ _ _) == 
   (AristotleAnyConcept id2 _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleAnyConcept where
  (AristotleAnyConcept id1 _ _ _ _ _ _ _) `compare` 
   (AristotleAnyConcept id2 _ _ _ _ _ _ _)  
      = id1 `compare` id2  

-- this wrapper does not exist. 
data AristotleAnyConceptJSON =
  AristotleAnyConceptJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleAnyConcept] 
           } deriving (Show,Generic) 

instance Eq AristotleAnyConceptJSON where 
  (AristotleAnyConceptJSON c1 _ _ _ ) == (AristotleAnyConceptJSON c2 _ _ _  )   
      = c1 == c2 

instance ToJSON AristotleAnyConceptJSON 
instance FromJSON AristotleAnyConceptJSON 

defaultAristotleAnyConceptJSON = AristotleAnyConceptJSON 0 Nothing Nothing [] 

data AristotleAnyConceptOut  = 
  AristotleAnyConceptOut { 
            ac2o_id :: String 
          , ac2o_uuid :: String
          , ac2o_name :: String
          , ac2o_version :: String           
          , ac2o_definition :: String
          , ac2o_short_definition :: String  
          , ac2o_absolute_url :: String
          , ac2o_expand_node_get_url :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleAnyConceptOut
instance ToNamedRecord AristotleAnyConceptOut
instance DefaultOrdered AristotleAnyConceptOut 

defaultAristotleAnyConceptOut = AristotleAnyConceptOut "" "" "" "" "" "" "" ""    

aac2aacOut :: AristotleAnyConcept -> AristotleAnyConceptOut  
aac2aacOut p = 
       AristotleAnyConceptOut 
          ((show.aac_id) p)  
          (aac_uuid p) 
          (trim (aac_name p))
          (aac_version p)          
          (trim (aac_definition p))
          (trim (aac_short_definition p)) 
          (aac_absolute_url p)
          (aac_expand_node_get_url p) 

-- ------------------------
-- Output 
-- extract all primary JSON objects
aacj2aac :: [Either String AristotleAnyConceptJSON] -> [AristotleAnyConcept] 
aacj2aac flist = concatMap results (rights flist) 

-- convert primary JSON objects into out list 
aac2Out :: [AristotleAnyConcept] -> [AristotleAnyConceptOut] 
aac2Out jList = map aac2aacOut jList   

-- All AC output collected into a single type   
data AristotleOutAC = 
  AristotleOutAC { 
            aoa_AristotleAnyItemType :: AristotleAnyItemType 
          , aoa_AristotleAnyConcept :: [AristotleAnyConcept]   
          , aoa_AristotleProcessResults :: AristotleProcessResults                     
          , aoa_AristotleAnyConceptOut :: [AristotleAnyConceptOut] 
          } deriving (Show,Generic)  

defaultAristotleOutAC = AristotleOutAC AnyConcept [] defaultAristotleProcessResults  [] 

populateAristotleOutAC :: AristotleAnyItemType -> [AristotleAnyConcept] -> AristotleProcessResults -> AristotleOutAC 
populateAristotleOutAC aait ps apr 
  = AristotleOutAC aait ps apr (aac2Out ps) 

-- full proc  
getAristotleAC :: AristotleAnyItemType -> [FetchResult] -> String -> String -> AristotleOutAC 
getAristotleAC aait fetched mts mte 
  = populateAristotleOutAC aait collected apr 
    where 
      parsed = map (rb2Aristotle.fetchedBody) fetched 
      collected = aacj2aac parsed 
      ajc = count (safeEither defaultAristotleAnyConceptJSON parsed) 
      apr = writeAPR aait mts mte ajc defaultAristotleAnyConceptJSON parsed collected  

-- it writes out a file if the count > 0  
writeAristotleAC :: AristotleOutAC -> String -> IO ()  
writeAristotleAC aoa fp 
  = do 
  if length (aoa_AristotleAnyConceptOut aoa) > 0 then 
    B.writeFile (aait2filename aait fp "") (encodeDefaultOrderedByName (aoa_AristotleAnyConceptOut aoa))  
  else return ()  

      where aait = (aoa_AristotleAnyItemType aoa) 

-- wrapper for the AC web request 
web2aoa :: W.Options -> Int -> IO AristotleOutAC 
web2aoa opts1 limitpage  = 
  do 
   let aait = AnyConcept 
-- fetching      
   mts <- messageTime "Start fetching: " 
   fetched <- crawl opts1 limitpage (Just (aait2http aait 0)) []  
   mte <- messageTime "End fetching:  " 
-- return  
   return (getAristotleAC aait fetched mts mte)  

-- wrapper for the AC file read and write  
file2aoa fAC aait = 
  do 
    B.writeFile (aait2filename aait "fileAristotle" "") (encodeDefaultOrderedByName ((aac2Out.aacj2aac) [(rb2Aristotle fAC)] ))  
 