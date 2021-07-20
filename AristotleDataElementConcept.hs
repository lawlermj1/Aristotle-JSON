{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleDataElementConcept
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Data Element Concept Class. 
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://dss.aristotlecloud.io/api/v4/ 

 -}
module AristotleDataElementConcept  
    (  
      
      AristotleDataElementConcept( .. ), 
      AristotleDataElementConceptJSON( .. ),       
      AristotleDataElementConceptOut( .. ),  

      rb2adecOut,
      rb2adec,  
      
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


data AristotleDataElementConcept  = 
  AristotleDataElementConcept { 
            adec_id :: Int 
          , adec_created :: UTCTime 
--            "created": "2020-11-12T15:35:40.990312+11:00",          
          , adec_modified :: UTCTime 
          , adec_uuid :: String
          , adec_name :: String
          , adec_definition :: String
          , adec_stewardship_organisation :: Maybe String  
          , adec_workgroup :: Maybe Int
          , adec_version :: Maybe String 
          , adec_references :: Maybe String 
          , adec_origin_URI :: Maybe String
          , adec_origin :: Maybe String 
          , adec_comments :: Maybe String
          , adec_objectClass :: Maybe String   
          , adec_property :: Maybe String                    
          , adec_conceptualDomain :: Maybe String          
          , adec_metadatareferencelink_set :: [AristotleMetadatarReferenceLink] 
          , adec_slots :: [AristotleSlot] 
          , adec_customvalue_set :: [AristotleCustomValue] 
          , adec_org_records :: [AristotleOrgRecord]           
          , adec_identifiers :: [AristotleIdentifier]   
          } deriving (Show,Generic) 

instance ToJSON AristotleDataElementConcept where 
    toJSON (AristotleDataElementConcept 
              adec_id adec_created adec_modified adec_uuid adec_name
              adec_definition adec_stewardship_organisation adec_workgroup adec_version 
              adec_references adec_origin_URI adec_origin adec_comments 
              adec_objectClass adec_property adec_conceptualDomain 
              adec_metadatareferencelink_set
              adec_slots adec_customvalue_set 
              adec_org_records 
              adec_identifiers
      ) = 
        object ["id" .= adec_id ,"created" .= adec_created  
               ,"modified" .= adec_modified ,"uuid" .= adec_uuid   
               ,"name" .= adec_name ,"definition" .= adec_definition 
               ,"stewardship_organisation" .= adec_stewardship_organisation 
               ,"workgroup" .= adec_workgroup  ,"version" .= adec_version     
               ,"references" .= adec_references  ,"origin_URI" .= adec_origin_URI     
               ,"origin" .= adec_origin  ,"comments" .= adec_comments     
               ,"objectClass" .= adec_objectClass  ,"property" .= adec_property  
               ,"conceptualDomain" .= adec_conceptualDomain  
               ,"metadatareferencelink_set" .= adec_metadatareferencelink_set                              
               ,"slots" .= adec_slots ,"customvalue_set" .= adec_customvalue_set  
               ,"org_records" .= adec_org_records 
               ,"identifiers" .= adec_identifiers                                                                     
               ]

instance FromJSON AristotleDataElementConcept where 
    parseJSON = withObject "AristotleDataElementConcept" $ \v -> AristotleDataElementConcept  
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
        <*> v .:? "objectClass"
        <*> v .:? "property"
        <*> v .:? "conceptualDomain"      
        <*> v .: "metadatareferencelink_set"                
        <*> v .: "slots"        
        <*> v .: "customvalue_set" 
        <*> v .: "org_records"         
        <*> v .: "identifiers" 

instance Eq AristotleDataElementConcept where
  (AristotleDataElementConcept id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) == 
    (AristotleDataElementConcept id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleDataElementConcept where
  (AristotleDataElementConcept id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) `compare` 
    (AristotleDataElementConcept id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 `compare` id2  


data AristotleDataElementConceptJSON =
  AristotleDataElementConceptJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleDataElementConcept] 
           } deriving (Show,Generic) 

instance Eq AristotleDataElementConceptJSON where 
  (AristotleDataElementConceptJSON c1 n1 p1 r1 ) == (AristotleDataElementConceptJSON c2 n2 p2 r2 )   
      = c1 == c2  
--     = c1 == c2 && n1 == n2 && p1 == p2  && r1 == r2          

instance ToJSON AristotleDataElementConceptJSON 
instance FromJSON AristotleDataElementConceptJSON 

defaultAristotleDataElementConceptJSON = AristotleDataElementConceptJSON 0 Nothing Nothing [] 

data AristotleDataElementConceptOut  = 
  AristotleDataElementConceptOut { 
            dec2o_id :: String 
          , dec2o_created :: String
          , dec2o_modified :: String
          , dec2o_uuid :: String
          , dec2o_name :: String
          , dec2o_definition :: String
          , dec2o_stewardship_organisation :: String  
          , dec2o_workgroup :: String
          , dec2o_version :: String 
          , dec2o_references :: String 
          , dec2o_origin_URI :: String
          , dec2o_origin :: String 
          , dec2o_comments :: String
          , dec2o_objectClass :: String
          , dec2o_property :: String    
          , dec2o_conceptualDomain :: String                    
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleDataElementConceptOut
instance ToNamedRecord AristotleDataElementConceptOut
instance DefaultOrdered AristotleDataElementConceptOut 

defaultAristotleDataElementConceptOut = AristotleDataElementConceptOut "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" 

adec2adecOut :: AristotleDataElementConcept -> AristotleDataElementConceptOut  
adec2adecOut p = 
       AristotleDataElementConceptOut 
          ((show.adec_id) p)  
          (showUTCTimeDate (adec_created p))          
          (showUTCTimeDate (adec_modified p))  
          (adec_uuid p) 
          (trim (adec_name p))
          (trim (adec_definition p)) 
          (fromMaybe "" (adec_stewardship_organisation p)) 
          (show (fromMaybe 0 (adec_workgroup p))) 
          (fromMaybe "" (adec_version p))
          (trim (fromMaybe "" (adec_references p))) 
          (fromMaybe "" (adec_origin_URI p))
          (fromMaybe "" (adec_origin p))
          (trim (fromMaybe "" (adec_comments p))) 
          (fromMaybe "" (adec_objectClass p))  
          (fromMaybe "" (adec_property p))  
          (fromMaybe "" (adec_conceptualDomain p))  


-- extract the list 
getAristotleDataElementConcept :: AristotleDataElementConceptJSON -> [AristotleDataElementConcept] 
getAristotleDataElementConcept jp = sort (results jp) 

-- convert to out  
allAristotleDataElementConceptOut :: AristotleDataElementConceptJSON -> [AristotleDataElementConceptOut] 
allAristotleDataElementConceptOut jp = map adec2adecOut (getAristotleDataElementConcept jp) 

-- parse 
rb2adec :: ByteString -> Either String AristotleDataElementConceptJSON   
rb2adec = (eitherDecode.ctlChar2SpacesBS) 

-- full extraction from byte string to class out
rb2adecOut :: ByteString -> [AristotleDataElementConceptOut] 
rb2adecOut rb 
  = allAristotleDataElementConceptOut (fromRight defaultAristotleDataElementConceptJSON (rb2adec rb)) 
