{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleValueDomain
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Value Domain Class.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://dss.aristotlecloud.io/api/v4/

 -}
module AristotleValueDomain  
    (  
      
      AristotleValueDomain( .. ), 
      AristotleValueDomainJSON( .. ),       
      AristotleValueDomainOut( .. ),  

      rb2avdOut, 
      rb2avd, 

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


data AristotleValueDomain  = 
  AristotleValueDomain { 
            avd_id :: Int 
          , avd_created :: UTCTime 
--            "created": "2020-11-12T15:35:40.990312+11:00",          
          , avd_modified :: UTCTime 
          , avd_uuid :: String
          , avd_name :: String
          , avd_definition :: String
          , avd_stewardship_organisation :: String  
          , avd_workgroup :: Int
          , avd_version :: String 
          , avd_references :: String 
          , avd_origin_URI :: String
          , avd_origin :: String 
          , avd_comments :: String
          , avd_data_type :: Maybe String   
          , avd_format :: Maybe String  
          , avd_maximum_length :: Maybe Int
          , avd_unit_of_measure :: Maybe String 
          , avd_conceptual_domain :: Maybe String      
          , avd_classification_scheme :: Maybe String              
          , avd_representation_class :: Maybe Int
          , avd_description :: String                      
          , avd_metadatareferencelink_set :: [AristotleMetadatarReferenceLink] 
          , avd_slots :: [AristotleSlot] 
          , avd_customvalue_set :: [AristotleCustomValue] 
          , avd_org_records :: [AristotleOrgRecord]           
          , avd_identifiers :: [AristotleIdentifier]   
          } deriving (Show,Generic) 

instance ToJSON AristotleValueDomain where 
    toJSON (AristotleValueDomain 
              avd_id avd_created avd_modified avd_uuid avd_name
              avd_definition avd_stewardship_organisation avd_workgroup avd_version 
              avd_references avd_origin_URI avd_origin avd_comments 
              avd_data_type avd_format avd_maximum_length 
              avd_unit_of_measure avd_conceptual_domain 
              avd_classification_scheme avd_representation_class 
              avd_description 
              avd_metadatareferencelink_set                               
              avd_slots avd_customvalue_set 
              avd_org_records
              avd_identifiers
      ) = 
        object ["id" .= avd_id ,"created" .= avd_created  
               ,"modified" .= avd_modified ,"uuid" .= avd_uuid   
               ,"name" .= avd_name ,"definition" .= avd_definition 
               ,"stewardship_organisation" .= avd_stewardship_organisation 
               ,"workgroup" .= avd_workgroup  ,"version" .= avd_version     
               ,"references" .= avd_references  ,"origin_URI" .= avd_origin_URI     
               ,"origin" .= avd_origin  ,"comments" .= avd_comments 
               ,"data_type" .= avd_data_type  ,"format" .= avd_format  
               ,"maximum_length" .= avd_maximum_length  ,"unit_of_measure" .= avd_unit_of_measure    
               ,"conceptual_domain" .= avd_conceptual_domain  ,"classification_scheme" .= avd_classification_scheme 
               ,"description" .= avd_description  
               ,"metadatareferencelink_set" .= avd_metadatareferencelink_set                
               ,"slots" .= avd_slots ,"customvalue_set" .= avd_customvalue_set  
               ,"org_records" .= avd_org_records  
               ,"identifiers" .= avd_identifiers                                                                     
               ]

instance FromJSON AristotleValueDomain where 
    parseJSON = withObject "AristotleValueDomain" $ \v -> AristotleValueDomain  
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
        <*> v .:? "data_type"
        <*> v .:? "format"
        <*> v .:? "maximum_length" 
        <*> v .:? "unit_of_measure"
        <*> v .:? "conceptual_domain"
        <*> v .:? "classification_scheme"
        <*> v .:? "representation_class"   
        <*> v .: "description"  
        <*> v .: "metadatareferencelink_set"         
        <*> v .: "slots"        
        <*> v .: "customvalue_set" 
        <*> v .: "org_records"         
        <*> v .: "identifiers" 

instance Eq AristotleValueDomain where
  (AristotleValueDomain id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) == 
    (AristotleValueDomain id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleValueDomain where
  (AristotleValueDomain id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) `compare` 
    (AristotleValueDomain id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 `compare` id2  


data AristotleValueDomainJSON =
  AristotleValueDomainJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleValueDomain] 
           } deriving (Show,Generic) 

instance Eq AristotleValueDomainJSON where 
  (AristotleValueDomainJSON c1 n1 p1 r1 ) == (AristotleValueDomainJSON c2 n2 p2 r2 )   
      = c1 == c2  
--     = c1 == c2 && n1 == n2 && p1 == p2  && r1 == r2          

instance ToJSON AristotleValueDomainJSON 
instance FromJSON AristotleValueDomainJSON 

defaultAristotleValueDomainJSON = AristotleValueDomainJSON 0 Nothing Nothing [] 

data AristotleValueDomainOut  = 
  AristotleValueDomainOut { 
            vd2o_id :: String 
          , vd2o_created :: String
          , vd2o_modified :: String
          , vd2o_uuid :: String
          , vd2o_name :: String
          , vd2o_definition :: String
          , vd2o_stewardship_organisation :: String  
          , vd2o_workgroup :: String
          , vd2o_version :: String 
          , vd2o_references :: String 
          , vd2o_origin_URI :: String
          , vd2o_origin :: String 
          , vd2o_comments :: String
          , vd2o_data_type :: String
          , vd2o_format :: String 
          , vd2o_maximum_length :: String 
          , vd2o_unit_of_measure :: String 
          , vd2o_conceptual_domain :: String 
          , vd2o_classification_scheme :: String 
          , vd2o_representation_class :: String 
          , vd2o_description :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleValueDomainOut
instance ToNamedRecord AristotleValueDomainOut
instance DefaultOrdered AristotleValueDomainOut 

defaultAristotleValueDomainOut = AristotleValueDomainOut "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""  

avd2avdOut :: AristotleValueDomain -> AristotleValueDomainOut  
avd2avdOut p = 
       AristotleValueDomainOut 
          ((show.avd_id) p)  
          (showUTCTimeDate (avd_created p))          
          (showUTCTimeDate (avd_modified p))  
          (avd_uuid p) 
          (trim (avd_name p)) 
          (trim (avd_definition p)) 
          (avd_stewardship_organisation p)
          ((show.avd_workgroup) p)
          (avd_version p)
          (trim (avd_references p))
          (avd_origin_URI p)
          (trim (avd_origin p))
          (trim (avd_comments p)) 
          (fromMaybe "" (avd_data_type p))  
          (fromMaybe "" (avd_format p))  
          (show (fromMaybe 0 (avd_maximum_length p)))      
          (fromMaybe "" (avd_unit_of_measure p))               
          (fromMaybe "" (avd_conceptual_domain p)) 
          (fromMaybe "" (avd_classification_scheme p)) 
          (show (fromMaybe 0 (avd_representation_class p)))  
          (avd_description p)  

-- extract the list 
getAristotleValueDomain :: AristotleValueDomainJSON -> [AristotleValueDomain] 
getAristotleValueDomain jp = sort (results jp) 

-- convert to out  
allAristotleValueDomainOut :: AristotleValueDomainJSON -> [AristotleValueDomainOut] 
allAristotleValueDomainOut jp = map avd2avdOut (getAristotleValueDomain jp) 

-- parse 
rb2avd :: ByteString -> Either String AristotleValueDomainJSON    
rb2avd = (eitherDecode.ctlChar2SpacesBS) 

-- full extraction from byte string to class out
rb2avdOut :: ByteString -> [AristotleValueDomainOut] 
rb2avdOut rb 
  = allAristotleValueDomainOut (fromRight defaultAristotleValueDomainJSON (rb2avd rb)) 
