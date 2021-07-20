{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleDataElement
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Data Element Class.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://dss.aristotlecloud.io/api/v4/ 

 -}
module AristotleDataElement  
    (  
      
      AristotleDataElement( .. ), 
      AristotleDataElementJSON( .. ),       
      AristotleDataElementOut( .. ),  

      rb2adeOut, 
      rb2ade, 
      
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


data AristotleDataElement  = 
  AristotleDataElement { 
            ade_id :: Int 
--          , ade_created :: String
          , ade_created :: UTCTime           
--            "created": "2020-11-12T15:35:40.990312+11:00",          
          , ade_modified :: UTCTime 
          , ade_uuid :: String
          , ade_name :: String
          , ade_definition :: String
          , ade_stewardship_organisation :: Maybe String  
          , ade_workgroup :: Maybe Int
          , ade_version :: Maybe String 
          , ade_references :: Maybe String 
          , ade_origin_URI :: Maybe String
          , ade_origin :: Maybe String 
          , ade_comments :: Maybe String
          , ade_dataElementConcept :: Maybe String
          , ade_valueDomain :: Maybe String          
          , ade_metadatareferencelink_set :: [AristotleMetadatarReferenceLink] 
          , ade_slots :: [AristotleSlot] 
          , ade_customvalue_set :: [AristotleCustomValue] 
          , ade_org_records :: [AristotleOrgRecord]           
          , ade_identifiers :: [AristotleIdentifier]   
          } deriving (Show,Generic) 

instance ToJSON AristotleDataElement where 
    toJSON (AristotleDataElement 
              ade_id ade_created ade_modified ade_uuid ade_name
              ade_definition ade_stewardship_organisation ade_workgroup ade_version 
              ade_references ade_origin_URI ade_origin ade_comments 
              ade_dataElementConcept ade_valueDomain
              ade_metadatareferencelink_set
              ade_slots ade_customvalue_set 
              ade_org_records
              ade_identifiers
      ) = 
        object ["id" .= ade_id ,"created" .= ade_created  
               ,"modified" .= ade_modified ,"uuid" .= ade_uuid   
               ,"name" .= ade_name ,"definition" .= ade_definition 
               ,"stewardship_organisation" .= ade_stewardship_organisation 
               ,"workgroup" .= ade_workgroup  ,"version" .= ade_version     
               ,"references" .= ade_references  ,"origin_URI" .= ade_origin_URI     
               ,"origin" .= ade_origin  ,"comments" .= ade_comments     
               ,"dataElementConcept" .= ade_dataElementConcept  
               ,"valueDomain" .= ade_valueDomain 
               ,"metadatareferencelink_set" .= ade_metadatareferencelink_set                 
               ,"slots" .= ade_slots ,"customvalue_set" .= ade_customvalue_set
               ,"org_records" .= ade_org_records                   
               ,"identifiers" .= ade_identifiers                                                                     
               ]

instance FromJSON AristotleDataElement where 
    parseJSON = withObject "AristotleDataElement" $ \v -> AristotleDataElement  
        <$> v .: "id" 
        <*> v .: "created" 
        <*> v .: "modified" 
        <*> v .: "uuid" 
        <*> v .: "name" 
        <*> v .: "definition" 
        <*> v .:? "stewardship_organisation" 
        <*> v .:? "workgroup" 
        <*> v .:? "version"
        <*> v .:? "references" 
        <*> v .:? "origin_URI" 
        <*> v .:? "origin" 
        <*> v .:? "comments"
        <*> v .:? "dataElementConcept"
        <*> v .:? "valueDomain"
        <*> v .: "metadatareferencelink_set"   
        <*> v .: "slots"              
        <*> v .: "customvalue_set" 
        <*> v .: "org_records"         
        <*> v .: "identifiers" 

instance Eq AristotleDataElement where
  (AristotleDataElement id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) == 
    (AristotleDataElement id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleDataElement where
  (AristotleDataElement id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) `compare` 
    (AristotleDataElement id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 `compare` id2  


data AristotleDataElementJSON =
  AristotleDataElementJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleDataElement] 
           } deriving (Show,Generic) 

instance Eq AristotleDataElementJSON where 
  (AristotleDataElementJSON c1 n1 p1 r1 ) == (AristotleDataElementJSON c2 n2 p2 r2 )   
      = c1 == c2  
--     = c1 == c2 && n1 == n2 && p1 == p2  && r1 == r2          

instance ToJSON AristotleDataElementJSON 
instance FromJSON AristotleDataElementJSON 

defaultAristotleDataElementJSON = AristotleDataElementJSON 0 Nothing Nothing [] 

data AristotleDataElementOut  = 
  AristotleDataElementOut { 
            de2o_id :: String 
          , de2o_created :: String
          , de2o_modified :: String
          , de2o_uuid :: String
          , de2o_name :: String
          , de2o_definition :: String
          , de2o_stewardship_organisation :: String  
          , de2o_workgroup :: String
          , de2o_version :: String 
          , de2o_references :: String 
          , de2o_origin_URI :: String
          , de2o_origin :: String 
          , de2o_comments :: String
          , de2o_dataElementConcepts :: String
          , de2o_valueDomain :: String          
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleDataElementOut
instance ToNamedRecord AristotleDataElementOut
instance DefaultOrdered AristotleDataElementOut 

defaultAristotleDataElementOut = AristotleDataElementOut "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" 

ade2adeOut :: AristotleDataElement -> AristotleDataElementOut  
ade2adeOut p = 
       AristotleDataElementOut 
          ((show.ade_id) p)  
          (showUTCTimeDate (ade_created p))           
          (showUTCTimeDate (ade_modified p)) 
          (ade_uuid p) 
          (trim (ade_name p)) 
          (trim (ade_definition p)) 
          (fromMaybe "" (ade_stewardship_organisation p))
          (show (fromMaybe 0 (ade_workgroup p)))  
          (fromMaybe "" (ade_version p))
          (trim (fromMaybe "" (ade_references p)))
          (fromMaybe "" (ade_origin_URI p))
          (fromMaybe "" (ade_origin p))
          (fromMaybe "" (ade_comments p)) 
          (fromMaybe "" (ade_dataElementConcept p)) 
          (fromMaybe "" (ade_valueDomain p))  


-- extract the body list 
getAristotleDataElement :: AristotleDataElementJSON -> [AristotleDataElement] 
getAristotleDataElement jp = sort (results jp) 

-- convert to out type 
allAristotleDataElementOut :: AristotleDataElementJSON -> [AristotleDataElementOut] 
allAristotleDataElementOut jp = map ade2adeOut (getAristotleDataElement jp) 

-- parse 
rb2ade :: ByteString -> Either String AristotleDataElementJSON   
rb2ade = (eitherDecode.ctlChar2SpacesBS) 

-- full extraction from byte string to class out
rb2adeOut :: ByteString -> [AristotleDataElementOut] 
rb2adeOut rb 
  = allAristotleDataElementOut (fromRight defaultAristotleDataElementJSON (rb2ade rb)) 
