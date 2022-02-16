{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleObjectClassSpecialisation
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Object Class.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/ 

removed common AnyItem fields, and retained only unique fields and child table 

aocs_objectclassspecialisationnarrowerclass_set :: [AristotleObjectClassSpecialisationNarrowerClass]           

 -}
module AristotleObjectClassSpecialisation  
    (  
      
      AristotleOutOCS( .. ), 

      writeAristotleOCS, 
      web2aoo, 
      file2aoo, 

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

-- not sure what this means, but ... 
-- "order": 0,                     "narrower_class": 2340
data AristotleObjectClassSpecialisationNarrowerClass  = 
  AristotleObjectClassSpecialisationNarrowerClass { 
            aocsnc_order :: Int  
          , aocsnc_narrower_class :: Maybe Int
          } deriving (Show,Generic) 

instance ToJSON AristotleObjectClassSpecialisationNarrowerClass where 
    toJSON (AristotleObjectClassSpecialisationNarrowerClass aocsnc_order aocsnc_narrower_class  ) = 
        object ["order" .= aocsnc_order ,"narrower_class" .= aocsnc_narrower_class ]

instance FromJSON AristotleObjectClassSpecialisationNarrowerClass where 
    parseJSON = withObject "AristotleObjectClassSpecialisationNarrowerClass" $ 
      \v -> AristotleObjectClassSpecialisationNarrowerClass  
        <$> v .: "order" 
        <*> v .:? "narrower_class" 

defaultAristotleObjectClassSpecialisationNarrowerClass = AristotleObjectClassSpecialisationNarrowerClass 0 Nothing  

data AristotleObjectClassSpecialisationNarrowerClassOut  = 
  AristotleObjectClassSpecialisationNarrowerClassOut { 
            ocsnc2o_concept_id :: String 
          , ocsnc2o_uuid :: String 
          , ocsnc2o_order :: String 
          , ocsnc2o_narrower_class :: String
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleObjectClassSpecialisationNarrowerClassOut
instance ToNamedRecord AristotleObjectClassSpecialisationNarrowerClassOut
instance DefaultOrdered AristotleObjectClassSpecialisationNarrowerClassOut 

defaultAristotleObjectClassSpecialisationNarrowerClassOut = AristotleObjectClassSpecialisationNarrowerClassOut "" "" "" ""  

aocsnc2acvsOut :: Int -> String -> [AristotleObjectClassSpecialisationNarrowerClass] 
  -> [AristotleObjectClassSpecialisationNarrowerClassOut] 
aocsnc2acvsOut concept uuid ps = 
    map (\p -> AristotleObjectClassSpecialisationNarrowerClassOut  
                 (show concept) 
                 uuid 
                 (show (aocsnc_order p)) 
                 (show (fromMaybe 0 (aocsnc_narrower_class p))) 
            ) ps

-- Primary parent - all boilerplate fields are on Any Item - just retain id for joining 
data AristotleObjectClassSpecialisation  = 
  AristotleObjectClassSpecialisation { 
            aocs_id :: Int 
          , aocs_uuid :: String
          , aocs_objectclassspecialisationnarrowerclass_set :: [AristotleObjectClassSpecialisationNarrowerClass]  
          } deriving (Show,Generic) 

instance ToJSON AristotleObjectClassSpecialisation where 
    toJSON (AristotleObjectClassSpecialisation aocs_id aocs_uuid 
              aocs_objectclassspecialisationnarrowerclass_set 
      ) = 
        object ["id" .= aocs_id ,"uuid" .= aocs_uuid,    
        "objectclassspecialisationnarrowerclass_set" .= aocs_objectclassspecialisationnarrowerclass_set ]

instance FromJSON AristotleObjectClassSpecialisation where 
    parseJSON = withObject "AristotleObjectClassSpecialisation" $ 
      \v -> AristotleObjectClassSpecialisation  
        <$> v .: "id" 
        <*> v .: "uuid" 
        <*> v .: "objectclassspecialisationnarrowerclass_set" 

instance Eq AristotleObjectClassSpecialisation where
  (AristotleObjectClassSpecialisation id1 _ _ ) == 
   (AristotleObjectClassSpecialisation id2 _ _  )  
      = id1 == id2  

instance Ord AristotleObjectClassSpecialisation where
  (AristotleObjectClassSpecialisation id1 _ _ ) `compare` 
   (AristotleObjectClassSpecialisation id2 _ _ )  
      = id1 `compare` id2  

data AristotleObjectClassSpecialisationJSON =
  AristotleObjectClassSpecialisationJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleObjectClassSpecialisation] 
           } deriving (Show,Generic) 

instance Eq AristotleObjectClassSpecialisationJSON where 
  (AristotleObjectClassSpecialisationJSON c1 _ _ _ ) == (AristotleObjectClassSpecialisationJSON c2 _ _ _ )   
      = c1 == c2 

instance ToJSON AristotleObjectClassSpecialisationJSON 
instance FromJSON AristotleObjectClassSpecialisationJSON 

defaultAristotleObjectClassSpecialisationJSON = AristotleObjectClassSpecialisationJSON 0 Nothing Nothing [] 

data AristotleObjectClassSpecialisationOut  = 
  AristotleObjectClassSpecialisationOut { 
            ocs2o_id :: String 
          , ocs2o_uuid :: String
          , ocs2o_objectclassspecialisationnarrowerclass_set :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleObjectClassSpecialisationOut
instance ToNamedRecord AristotleObjectClassSpecialisationOut
instance DefaultOrdered AristotleObjectClassSpecialisationOut 

defaultAristotleObjectClassSpecialisationOut = AristotleObjectClassSpecialisationOut "" "" "" 

-- ------------------------------
-- Testing 
-- Cardinality 
data AristotleObjectClassSpecialisationCardinality  = 
  AristotleObjectClassSpecialisationCardinality { 
            aocs_objectclassspecialisationnarrowerclass_set_max :: Int 
          } deriving (Show,Generic) 

defaultAristotleObjectClassSpecialisationCardinality = AristotleObjectClassSpecialisationCardinality 0 

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
aocsfold :: AristotleObjectClassSpecialisationCardinality -> AristotleObjectClassSpecialisation -> AristotleObjectClassSpecialisationCardinality
aocsfold card aocs = 
    AristotleObjectClassSpecialisationCardinality 
        (max (aocs_objectclassspecialisationnarrowerclass_set_max card) (length (aocs_objectclassspecialisationnarrowerclass_set aocs))) 

-- let cards = foldl aocsfold defaultAristotleObjectClassSpecialisationCardinality aList 
-- print cards 

-- ------------------------------
-- Output 

-- extract all primary JSON objects
aocsj2aocs :: [Either String AristotleObjectClassSpecialisationJSON] -> [AristotleObjectClassSpecialisation] 
aocsj2aocs flist = concatMap results (rights flist) 

-- convert primary JSON objects into out list 
aocs2Out :: [AristotleObjectClassSpecialisation] -> [AristotleObjectClassSpecialisationOut] 
aocs2Out jList = map aocs2aocsOut jList  

-- 
aocs2aocsOut :: AristotleObjectClassSpecialisation -> AristotleObjectClassSpecialisationOut  
aocs2aocsOut p = 
       AristotleObjectClassSpecialisationOut 
          ((show.aocs_id) p) 
          (aocs_uuid p) 
          (concatMap show (aocs_objectclassspecialisationnarrowerclass_set p))    

-- convert JSON objects into AristotleObjectClassSpecialisationNarrowerClass dependent list 
aocs2aocsncOut :: [AristotleObjectClassSpecialisation] -> [AristotleObjectClassSpecialisationNarrowerClassOut] 
aocs2aocsncOut ps = concatMap (\p -> aocsnc2acvsOut (aocs_id p) (aocs_uuid p) (aocs_objectclassspecialisationnarrowerclass_set p)) ps 

-- All OCS output collected into a single type   
data AristotleOutOCS = 
  AristotleOutOCS { 
            aoo_AristotleAnyItemType :: AristotleAnyItemType 
          , aoo_AristotleObjectClassSpecialisation :: [AristotleObjectClassSpecialisation]   
          , aoo_AristotleProcessResults :: AristotleProcessResults                     
          , aoo_AristotleObjectClassSpecialisationOut :: [AristotleObjectClassSpecialisationOut] 
          , aoo_AristotleObjectClassSpecialisationNarrowerClassOut :: [AristotleObjectClassSpecialisationNarrowerClassOut] 
          } deriving (Show,Generic)  

defaultAristotleOutOCS = AristotleOutOCS ObjectClassSpecialisation [] defaultAristotleProcessResults  [] [] 

populateAristotleOutOCS :: AristotleAnyItemType -> [AristotleObjectClassSpecialisation] -> AristotleProcessResults -> AristotleOutOCS 
populateAristotleOutOCS aait ps apr 
  = AristotleOutOCS aait ps apr (aocs2Out ps) (aocs2aocsncOut ps) 

-- full proc  
getAristotleOCS :: AristotleAnyItemType -> [FetchResult] -> String -> String -> AristotleOutOCS 
getAristotleOCS aait fetched mts mte 
  = populateAristotleOutOCS aait collected apr 
    where 
      parsed = map (rb2Aristotle.fetchedBody) fetched 
      collected = aocsj2aocs parsed 
      ajc = count (safeEither defaultAristotleObjectClassSpecialisationJSON parsed) 
      apr = writeAPR aait mts mte ajc defaultAristotleObjectClassSpecialisationJSON parsed collected  

-- it writes out a file if the count > 0  
writeAristotleOCS :: AristotleOutOCS -> String -> IO ()  
writeAristotleOCS aoo fp 
  = do 
  if length (aoo_AristotleObjectClassSpecialisationOut aoo) > 0 then 
    B.writeFile (aait2filename aait fp "") (encodeDefaultOrderedByName (aoo_AristotleObjectClassSpecialisationOut aoo))  
  else return ()   

  if length (aoo_AristotleObjectClassSpecialisationNarrowerClassOut aoo) > 0 then 
    B.writeFile (aait2filename aait fp "NarrowerClassOut") (encodeDefaultOrderedByName (aoo_AristotleObjectClassSpecialisationNarrowerClassOut aoo))  
  else return ()  
      where aait = (aoo_AristotleAnyItemType aoo) 

-- wrapper for the OCS web request 
web2aoo :: W.Options -> Int -> IO AristotleOutOCS 
web2aoo opts1 limitpage  = 
  do 
   let aait = ObjectClassSpecialisation 
-- fetching      
   mts <- messageTime "Start fetching: " 
   fetched <- crawl opts1 limitpage (Just (aait2http aait 100)) []  
   mte <- messageTime "End fetching:  " 
-- return  
   return (getAristotleOCS aait fetched mts mte)  

-- wrapper for the OCS file read and write  
file2aoo adtBS aait = 
  do 
    B.writeFile (aait2filename aait "fileAristotle" "") (encodeDefaultOrderedByName ((aocs2Out.aocsj2aocs) [(rb2Aristotle adtBS)] ))  
