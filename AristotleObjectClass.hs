{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleObjectClass
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Object Class.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://dss.aristotlecloud.io/api/v4/ 

 -}
module AristotleObjectClass  
    (  
      
      AristotleObjectClass( .. ), 
      AristotleObjectClassJSON( .. ),       
      AristotleObjectClassOut( .. ),  

      rb2aocOut, 
      rb2aoc, 

     ) where
    
import Data.Aeson
import Data.Either 
import Data.List 
import Data.Maybe  
import Data.Time  
import GHC.Generics
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered) 
import Data.ByteString.Lazy (ByteString) 

import CassavaUtils 
import AristotleCommon  


data AristotleObjectClass  = 
  AristotleObjectClass { 
            aoc_id :: Int 
          , aoc_created :: UTCTime 
--            "created": "2020-11-12T15:35:40.990312+11:00",          
          , aoc_modified :: UTCTime
          , aoc_uuid :: String
          , aoc_name :: String
          , aoc_definition :: String
          , aoc_stewardship_organisation :: String  
          , aoc_workgroup :: Int
          , aoc_version :: String 
          , aoc_references :: String 
          , aoc_origin_URI :: String
          , aoc_origin :: String 
          , aoc_comments :: String
          , aoc_metadatareferencelink_set :: [AristotleMetadatarReferenceLink] 
          , aoc_slots :: [AristotleSlot] 
          , aoc_customvalue_set :: [AristotleCustomValue] 
          , aoc_org_records :: [AristotleOrgRecord]           
          , aoc_identifiers :: [AristotleIdentifier]   
          } deriving (Show,Generic) 

instance ToJSON AristotleObjectClass where 
    toJSON (AristotleObjectClass 
              aoc_id aoc_created aoc_modified aoc_uuid aoc_name
              aoc_definition aoc_stewardship_organisation aoc_workgroup aoc_version 
              aoc_references aoc_origin_URI aoc_origin aoc_comments 
              aoc_metadatareferencelink_set 
              aoc_slots aoc_customvalue_set 
              aoc_org_records
              aoc_identifiers
      ) = 
        object ["id" .= aoc_id ,"created" .= aoc_created  
               ,"modified" .= aoc_modified ,"uuid" .= aoc_uuid   
               ,"name" .= aoc_name ,"definition" .= aoc_definition 
               ,"stewardship_organisation" .= aoc_stewardship_organisation 
               ,"workgroup" .= aoc_workgroup  ,"version" .= aoc_version     
               ,"references" .= aoc_references  ,"origin_URI" .= aoc_origin_URI     
               ,"origin" .= aoc_origin  ,"comments" .= aoc_comments 
               ,"metadatareferencelink_set" .= aoc_metadatareferencelink_set                 
               ,"slots" .= aoc_slots ,"customvalue_set" .= aoc_customvalue_set  
               ,"org_records" .= aoc_org_records 
               ,"identifiers" .= aoc_identifiers                                                                     
               ]

instance FromJSON AristotleObjectClass where 
    parseJSON = withObject "AristotleObjectClass" $ \v -> AristotleObjectClass  
        <$> v .: "id" 
        <*> v .: "created" 
        <*> v .: "modified" 
        <*> v .: "uuid" 
        <*> v .: "name" 
        <*> v .: "definition" 
        <*> v .: "stewardship_organisation" 
        <*> v .: "workgroup" 
        <*> v .: "version"
        <*> v .: "references" 
        <*> v .: "origin_URI" 
        <*> v .: "origin" 
        <*> v .: "comments"
        <*> v .: "metadatareferencelink_set"        
        <*> v .: "slots"        
        <*> v .: "customvalue_set" 
        <*> v .: "org_records"         
        <*> v .: "identifiers" 

instance Eq AristotleObjectClass where
  (AristotleObjectClass id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) == 
   (AristotleObjectClass id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleObjectClass where
  (AristotleObjectClass id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) `compare` 
   (AristotleObjectClass id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 `compare` id2  


data AristotleObjectClassJSON =
  AristotleObjectClassJSON { 
            count :: Int
--          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleObjectClass] 
           } deriving (Show,Generic) 

instance Eq AristotleObjectClassJSON where 
  (AristotleObjectClassJSON c1 p1 r1 ) == (AristotleObjectClassJSON c2 p2 r2 )   
      = c1 == c2 

instance ToJSON AristotleObjectClassJSON 
instance FromJSON AristotleObjectClassJSON 

defaultAristotleObjectClassJSON = AristotleObjectClassJSON 0 Nothing [] 

data AristotleObjectClassOut  = 
  AristotleObjectClassOut { 
            oc2o_id :: String 
          , oc2o_created :: String
          , oc2o_modified :: String
          , oc2o_uuid :: String
          , oc2o_name :: String
          , oc2o_definition :: String
          , oc2o_stewardship_organisation :: String  
          , oc2o_workgroup :: String
          , oc2o_version :: String 
          , oc2o_references :: String 
          , oc2o_origin_URI :: String
          , oc2o_origin :: String 
          , oc2o_comments :: String
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleObjectClassOut
instance ToNamedRecord AristotleObjectClassOut
instance DefaultOrdered AristotleObjectClassOut 

defaultAristotleObjectClassOut = AristotleObjectClassOut "" "" "" "" "" "" "" "" "" "" "" "" ""   

-- ----------

-- extract the list 
getAristotleObjectClass :: AristotleObjectClassJSON -> [AristotleObjectClass] 
getAristotleObjectClass jp = sort (results jp) 

aoc2aocOut :: AristotleObjectClass -> AristotleObjectClassOut  
aoc2aocOut p = 
       AristotleObjectClassOut 
          ((show.aoc_id) p)  
          (showUTCTimeDate (aoc_created p))           
          (showUTCTimeDate (aoc_modified p))  
          (aoc_uuid p) 
          (trim (aoc_name p))
          (trim (aoc_definition p))
          (aoc_stewardship_organisation p)
          ((show.aoc_workgroup) p)
          (aoc_version p)
          (trim (aoc_references p))
          (aoc_origin_URI p)
          (trim (aoc_origin p))
          (trim (aoc_comments p))  

-- convert to out  
allAristotleObjectClassOut :: AristotleObjectClassJSON -> [AristotleObjectClassOut] 
allAristotleObjectClassOut jp = map aoc2aocOut (getAristotleObjectClass jp) 

-- parse 
rb2aoc :: ByteString -> Either String AristotleObjectClassJSON  
rb2aoc = (eitherDecode.ctlChar2SpacesBS) 

-- full extraction from byte string to class out
rb2aocOut :: ByteString -> [AristotleObjectClassOut] 
rb2aocOut rb = allAristotleObjectClassOut (fromRight defaultAristotleObjectClassJSON (rb2aoc rb)) 
